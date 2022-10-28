module SharedLogic.DriverPool
  ( DriverPoolResult (..),
    calculateDriverPool,
    recalculateDriverPool,
    getDriverPool,
  )
where

import Beckn.External.Maps.Google as Google
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Booking as SRB
import Domain.Types.DriverPool
import qualified Domain.Types.FarePolicy.FareProduct as SFP
import qualified Domain.Types.Organization as SOrg
import Domain.Types.Person (Driver)
import qualified Domain.Types.TransporterConfig as STConf
import qualified Domain.Types.Vehicle as SV
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (id)
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.TransporterConfig as QTConf
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Metrics

data DriverPoolResult = DriverPoolResult
  { driverId :: Id Driver,
    distanceToPickup :: Meters,
    durationToPickup :: Seconds,
    variant :: Vehicle.Variant,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, HasCoordinates)

mkDriverPoolItem :: DriverPoolResult -> DriverPoolItem
mkDriverPoolItem DriverPoolResult {..} = DriverPoolItem {..}

driverPoolKey :: Id SRB.Booking -> Text
driverPoolKey = ("beckn:driverpool:" <>) . getId

getKeyRedis :: Redis.HedisFlow m r => Text -> m (Maybe [DriverPoolItem])
getKeyRedis = Redis.get

setExRedis :: Redis.HedisFlow m r => Text -> [DriverPoolItem] -> Int -> m ()
setExRedis = Redis.setExp

getDriverPool ::
  ( HasCacheConfig r,
    CoreMetrics m,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasGoogleCfg r
  ) =>
  Id SRB.Booking ->
  m SortedDriverPool
getDriverPool bookingId =
  getKeyRedis (driverPoolKey bookingId)
    >>= maybe calcDriverPool (pure . mkSortedDriverPool)
  where
    calcDriverPool = do
      booking <- QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
      let vehicleVariant = booking.vehicleVariant
          orgId = booking.providerId
      let pickupLoc = booking.fromLocation
      let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
          fareProductType = SRB.getFareProductType booking.bookingDetails
      mkSortedDriverPool . map mkDriverPoolItem <$> calculateDriverPool pickupLatLong orgId (Just vehicleVariant) fareProductType

recalculateDriverPool ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    CoreMetrics m,
    HasGoogleCfg r
  ) =>
  SRB.Booking ->
  m [DriverPoolResult]
recalculateDriverPool booking = do
  let pickupLoc = booking.fromLocation
      transporterId = booking.providerId
      vehicleVariant = booking.vehicleVariant
      fareProductType = SRB.getFareProductType booking.bookingDetails
  let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
  driverPoolResults <- calculateDriverPool pickupLatLong transporterId (Just vehicleVariant) fareProductType
  cancelledDrivers <- QRide.findAllCancelledByRBId booking.id <&> map (cast . (.driverId))
  let filteredDriverPoolResults = [x | x <- driverPoolResults, x.driverId `notElem` cancelledDrivers]
      filteredDriverPool = map mkDriverPoolItem filteredDriverPoolResults
  setExRedis (driverPoolKey booking.id) filteredDriverPool (60 * 10)
  return filteredDriverPoolResults

calculateDriverPool ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    CoreMetrics m,
    HasGoogleCfg r
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
    HasGoogleCfg r
  ) =>
  LatLong ->
  NonEmpty QP.NearestDriversResult ->
  m (NonEmpty DriverPoolResult)
buildDriverPoolResults pickup ndResults = do
  distDurs <- Google.getDistances (Just Google.CAR) (pickup :| []) ndResults
  return $ mkDriverPoolResult <$> distDurs
  where
    mkDriverPoolResult distDur = do
      let QP.NearestDriversResult {..} = distDur.destination
      DriverPoolResult
        { distanceToPickup = distDur.distance,
          durationToPickup = distDur.duration,
          ..
        }

filterOutDriversWithDistanceAboveThreshold ::
  ( EsqDBFlow m r,
    CoreMetrics m
  ) =>
  Integer ->
  NonEmpty DriverPoolResult ->
  m [DriverPoolResult]
filterOutDriversWithDistanceAboveThreshold threshold driverPoolResults = do
  pure $ NE.filter filterFunc driverPoolResults
  where
    filterFunc drPoolRes = drPoolRes.distanceToPickup <= fromIntegral threshold
