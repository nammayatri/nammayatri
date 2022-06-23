module SharedLogic.DriverPool (calculateDriverPool, recalculateDriverPool, getDriverPool) where

import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (LatLong))
import qualified Beckn.Types.MapSearch as GoogleMaps
import qualified Domain.Types.BusinessEvent as SB
import qualified Domain.Types.FareProduct as SFP
import qualified Domain.Types.Organization as SOrg
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.SearchReqLocation as SSReqLoc
import qualified Domain.Types.TransporterConfig as STConf
import qualified Domain.Types.Vehicle as SV
import EulerHS.Prelude hiding (id)
import Product.Location (getDistanceDuration, locationToLatLong)
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRideBooking
import qualified Storage.Queries.SearchReqLocation as QSReqLoc
import qualified Storage.Queries.TransporterConfig as QTConf
import Tools.Metrics
import Types.App (Driver)
import Types.Error
import Utils.Common

driverPoolKey :: Id SRB.RideBooking -> Text
driverPoolKey = ("beckn:driverpool:" <>) . getId

getDriverPool ::
  ( CoreMetrics m,
    EsqDBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasGoogleMaps m r
  ) =>
  Id SRB.RideBooking ->
  m [Id Driver]
getDriverPool rideBookingId =
  Redis.getKeyRedis (driverPoolKey rideBookingId)
    >>= maybe calcDriverPool (pure . map Id)
  where
    calcDriverPool = do
      rideBooking <- QRideBooking.findById rideBookingId >>= fromMaybeM (RideBookingDoesNotExist rideBookingId.getId)
      let vehicleVariant = rideBooking.vehicleVariant
          pickupPoint = rideBooking.fromLocationId
          orgId = rideBooking.providerId
          fareProductType = SRB.getFareProductType rideBooking.rideBookingDetails
      driverPoolResults <- calculateDriverPool pickupPoint orgId (Just vehicleVariant) fareProductType
      Esq.runTransaction $ traverse_ (QBE.logDriverInPoolEvent SB.ON_REALLOCATION (Just rideBooking.id)) driverPoolResults
      pure $ map (.driverId) driverPoolResults

recalculateDriverPool ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  Id SSReqLoc.SearchReqLocation ->
  Id SRB.RideBooking ->
  Id SOrg.Organization ->
  SV.Variant ->
  SFP.FareProductType ->
  m [QP.DriverPoolResult]
recalculateDriverPool pickupPoint rideBookingId transporterId vehicleVariant fareProductType = do
  driverPoolResults <- calculateDriverPool pickupPoint transporterId (Just vehicleVariant) fareProductType
  cancelledDrivers <- QRide.findAllCancelledByRBId rideBookingId <&> map (cast . (.driverId))
  let filteredDriverPoolResults = [x | x <- driverPoolResults, QP.driverId x `notElem` cancelledDrivers]
      filteredDriverPool = map (.driverId) filteredDriverPoolResults
  Redis.setExRedis (driverPoolKey rideBookingId) (getId <$> filteredDriverPool) (60 * 10)
  return filteredDriverPoolResults

calculateDriverPool ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  Id SSReqLoc.SearchReqLocation ->
  Id SOrg.Organization ->
  Maybe SV.Variant ->
  SFP.FareProductType ->
  m [QP.DriverPoolResult]
calculateDriverPool locId orgId variant fareProductType = do
  pickupLoc <- QSReqLoc.findById locId >>= fromMaybeM LocationNotFound
  let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
  radius <- getRadius
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPool" $
      QP.getNearestDrivers
        pickupLatLong
        radius
        orgId
        variant
        fareProductType
  approxDriverPool' <- mapM (addDuration pickupLatLong) approxDriverPool
  case approxDriverPool' of
    [] -> pure []
    (a : pprox) -> filterOutDriversWithDistanceAboveThreshold radius pickupLatLong (a :| pprox)
  where
    getRadius =
      QTConf.findValueByOrgIdAndKey orgId (STConf.ConfigKey "radius")
        >>= maybe
          (fromIntegral <$> asks (.defaultRadiusOfSearch))
          radiusFromTransporterConfig
    radiusFromTransporterConfig conf =
      fromMaybeM (InternalError "The radius is not a number.")
        . readMaybe
        . toString
        $ conf.value

addDuration ::
  ( EsqDBFlow m r,
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  LatLong ->
  QP.DriverPoolResult ->
  m QP.DriverPoolResult
addDuration pickup driverInPool = do
  distDur <- getDistanceDuration pickup $ locationToLatLong driverInPool
  case snd distDur of
    Nothing -> return driverInPool
    Just d -> return $ driverInPool {QP.durationToPickup = Just (fromInteger d :: Double)}

filterOutDriversWithDistanceAboveThreshold ::
  ( EsqDBFlow m r,
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  Integer ->
  LatLong ->
  NonEmpty QP.DriverPoolResult ->
  m [QP.DriverPoolResult]
filterOutDriversWithDistanceAboveThreshold threshold pickupLatLong driverPoolResults = do
  getDistanceResults <- GoogleMaps.getDistancesGeneral (Just GoogleMaps.CAR) driverPoolResults (pickupLatLong :| []) zipFunc Nothing
  pure $ map fst $ filter (filterFunc . snd) getDistanceResults
  where
    zipFunc dpRes _ estDist = (dpRes, estDist.distance)
    filterFunc estDist = getDistanceInMeter estDist <= fromIntegral threshold
