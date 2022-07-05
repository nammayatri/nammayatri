module SharedLogic.DriverPool
  ( DriverPoolResult (..),
    calculateDriverPool,
    recalculateDriverPool,
    getDriverPool,
  )
where

import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import qualified Beckn.Product.MapSearch.GoogleMaps as MapSearch
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (LatLong))
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.BookingLocation as DBLoc
import qualified Domain.Types.FareProduct as SFP
import qualified Domain.Types.Organization as SOrg
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.TransporterConfig as STConf
import qualified Domain.Types.Vehicle as SV
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.BookingLocation as QBLoc
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRideBooking
import qualified Storage.Queries.TransporterConfig as QTConf
import Tools.Metrics
import Types.App (Driver)
import Types.Error
import Utils.Common

data DriverPoolResult = DriverPoolResult
  { driverId :: Id Driver,
    distanceToDriver :: Meters,
    durationToPickup :: Seconds,
    variant :: Vehicle.Variant,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, MapSearch.HasCoordinates)

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
          orgId = rideBooking.providerId
      pickupLoc <- QBLoc.findById rideBooking.fromLocationId >>= fromMaybeM LocationNotFound
      let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
          fareProductType = SRB.getFareProductType rideBooking.rideBookingDetails
      map (.driverId) <$> calculateDriverPool pickupLatLong orgId (Just vehicleVariant) fareProductType

recalculateDriverPool ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  Id DBLoc.BookingLocation ->
  Id SRB.RideBooking ->
  Id SOrg.Organization ->
  SV.Variant ->
  SFP.FareProductType ->
  m [DriverPoolResult]
recalculateDriverPool pickupPoint rideBookingId transporterId vehicleVariant fareProductType = do
  pickupLoc <- QBLoc.findById pickupPoint >>= fromMaybeM LocationNotFound
  let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
  driverPoolResults <- calculateDriverPool pickupLatLong transporterId (Just vehicleVariant) fareProductType
  cancelledDrivers <- QRide.findAllCancelledByRBId rideBookingId <&> map (cast . (.driverId))
  let filteredDriverPoolResults = [x | x <- driverPoolResults, x.driverId `notElem` cancelledDrivers]
      filteredDriverPool = map (.driverId) filteredDriverPoolResults
  Redis.setExRedis (driverPoolKey rideBookingId) (getId <$> filteredDriverPool) (60 * 10)
  return filteredDriverPoolResults

calculateDriverPool ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  LatLong ->
  Id SOrg.Organization ->
  Maybe SV.Variant ->
  SFP.FareProductType ->
  m [DriverPoolResult]
calculateDriverPool pickupLatLong orgId variant fareProductType = do
  radius <- getRadius
  nearestDriversResult <-
    measuringDurationToLog INFO "calculateDriverPool" $
      QP.getNearestDrivers
        pickupLatLong
        radius
        orgId
        variant
        fareProductType
  case nearestDriversResult of
    [] -> pure []
    (a : xs) -> do
      approxDriverPool' <- buildDriverPoolResults pickupLatLong (a :| xs)
      filterOutDriversWithDistanceAboveThreshold radius approxDriverPool'
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

buildDriverPoolResults ::
  ( EsqDBFlow m r,
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  LatLong ->
  NonEmpty QP.NearestDriversResult ->
  m (NonEmpty DriverPoolResult)
buildDriverPoolResults pickup ndResults = do
  distDurs <- MapSearch.getDistances (Just MapSearch.CAR) (pickup :| []) ndResults Nothing
  return $ mkDriverPoolResult <$> distDurs
  where
    mkDriverPoolResult distDur = do
      let QP.NearestDriversResult {..} = distDur.destination
      DriverPoolResult
        { distanceToDriver = distDur.distance,
          durationToPickup = distDur.duration,
          ..
        }

filterOutDriversWithDistanceAboveThreshold ::
  ( EsqDBFlow m r,
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  Integer ->
  NonEmpty DriverPoolResult ->
  m [DriverPoolResult]
filterOutDriversWithDistanceAboveThreshold threshold driverPoolResults = do
  pure $ NE.filter filterFunc driverPoolResults
  where
    filterFunc drPoolRes = drPoolRes.distanceToDriver <= fromIntegral threshold
