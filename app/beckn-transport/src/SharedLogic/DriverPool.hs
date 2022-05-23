module SharedLogic.DriverPool (calculateDriverPool, recalculateDriverPool, getDriverPool) where

import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (LatLong))
import qualified Beckn.Types.MapSearch as GoogleMaps
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.FareProduct as SFP
import qualified Domain.Types.Organization as SOrg
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.SearchReqLocation as SSReqLoc
import qualified Domain.Types.TransporterConfig as STConf
import qualified Domain.Types.Vehicle as SV
import EulerHS.Prelude hiding (id)
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
    HasGoogleMaps m r c
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
      map (.driverId) <$> calculateDriverPool pickupPoint orgId (Just vehicleVariant) fareProductType

recalculateDriverPool ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    CoreMetrics m,
    HasGoogleMaps m r c
  ) =>
  Id SSReqLoc.SearchReqLocation ->
  Id SRB.RideBooking ->
  Id SOrg.Organization ->
  SV.Variant ->
  SFP.FareProductType ->
  m [Id Driver]
recalculateDriverPool pickupPoint rideBookingId transporterId vehicleVariant fareProductType = do
  driverPool <- map (.driverId) <$> calculateDriverPool pickupPoint transporterId (Just vehicleVariant) fareProductType
  cancelledDrivers <- QRide.findAllCancelledByRBId rideBookingId <&> map (cast . (.driverId))
  let filteredDriverPool = filter (`notElem` cancelledDrivers) driverPool
  Redis.setExRedis (driverPoolKey rideBookingId) (getId <$> filteredDriverPool) (60 * 10)
  return filteredDriverPool

calculateDriverPool ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    CoreMetrics m,
    HasGoogleMaps m r c
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
  case approxDriverPool of
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

filterOutDriversWithDistanceAboveThreshold ::
  ( EsqDBFlow m r,
    CoreMetrics m,
    HasGoogleMaps m r c
  ) =>
  Integer ->
  LatLong ->
  NonEmpty QP.DriverPoolResult ->
  m [QP.DriverPoolResult]
filterOutDriversWithDistanceAboveThreshold threshold pickupLatLong driverPoolResults = do
  getDistanceResults <- GoogleMaps.getDistances (Just GoogleMaps.CAR) originLatLongList (pickupLatLong :| []) Nothing
  return $ NE.filter (filterFunc getDistanceResults) driverPoolResults
  where
    originLatLongList =
      map (\dpRes -> LatLong dpRes.lat dpRes.lon) driverPoolResults
    filterFunc getDistanceResults dpRes = do
      let threshold' = fromIntegral threshold
      let mbGdRes = findGetDistanceResult getDistanceResults dpRes
      case mbGdRes of
        Nothing -> False
        Just gdRes -> (getDistanceInMeter gdRes.info.distance) <= threshold'

findGetDistanceResult ::
  [GoogleMaps.GetDistanceResult] ->
  QP.DriverPoolResult ->
  Maybe GoogleMaps.GetDistanceResult
findGetDistanceResult getDistanceResults driverPoolResult =
  find filterFunc getDistanceResults
  where
    filterFunc gdRes = do
      case gdRes.origin of
        GoogleMaps.Location origin ->
          origin.lat == driverPoolResult.lat
            && origin.lng == driverPoolResult.lon
        _ -> False
