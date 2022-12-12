module SharedLogic.DriverPool
  ( calculateDriverPool,
    getDriverPoolBatch,
    cleanDriverPoolBatches,
    module Reexport,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Data.List.Extra as List
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.FarePolicy.FareProduct as SFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.TransporterConfig as STConf
import qualified Domain.Types.Vehicle as SV
import Safe (atMay)
import SharedLogic.DriverPool.Config as Reexport
import SharedLogic.DriverPool.Types as Reexport
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.TransporterConfig as QTConf
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Maps as Google
import qualified Tools.Maps as Maps
import Tools.Metrics

driverPoolKey :: Id SRB.Booking -> Text
driverPoolKey bookingId = "DriverPool:BookingId-" <> bookingId.getId

driverPoolBatchKey :: Id SRB.Booking -> PoolRadiusStep -> PoolBatchNum -> Text
driverPoolBatchKey bookingId radiusStep batchNum = driverPoolKey bookingId <> ":RadiusStep-" <> show radiusStep <> ":BatchNum-" <> show batchNum

getDriverPoolBatch ::
  ( EncFlow m r,
    HasCacheConfig r,
    CoreMetrics m,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasDriverPoolConfig r
  ) =>
  Id SRB.Booking ->
  PoolRadiusStep ->
  PoolBatchNum ->
  m [DriverPoolResult]
getDriverPoolBatch bookingId radiusStep batchNum = do
  Redis.get (driverPoolBatchKey bookingId batchNum radiusStep)
    >>= flip maybe pure do
      driverPool <- calcDriverPool
      let sortedDriverPool = sortFunction driverPool
      driverPoolBatches <- splitToBatches sortedDriverPool
      cacheBatches driverPoolBatches
      return $ fromMaybe [] $ driverPoolBatches `atMay` fromIntegral batchNum
  where
    calcDriverPool = do
      booking <- QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
      let vehicleVariant = booking.vehicleVariant
          merchantId = booking.providerId
      let pickupLoc = booking.fromLocation
      let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
          fareProductType = SRB.getFareProductType booking.bookingDetails
      calculateDriverPool pickupLatLong merchantId (Just vehicleVariant) fareProductType radiusStep
    splitToBatches driverPool = do
      batchSize <- asks (.driverPoolCfg.driverBatchSize)
      return $ List.chunksOf batchSize driverPool
    cacheBatches =
      void . flip foldlM 0 \i batch -> do
        Redis.setExp (driverPoolBatchKey bookingId i radiusStep) batch (60 * 10)
        return (i + 1)
    sortFunction driverPool =
      driverPool -- it's random order by default

cleanDriverPoolBatches ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r
  ) =>
  Id SRB.Booking ->
  m ()
cleanDriverPoolBatches bookingId =
  Redis.delByPattern (driverPoolKey bookingId <> "*")

calculateDriverPool ::
  ( EncFlow m r,
    HasCacheConfig r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasDriverPoolConfig r,
    CoreMetrics m,
    HasCoordinates a
  ) =>
  a ->
  Id DM.Merchant ->
  Maybe SV.Variant ->
  SFP.FareProductType ->
  PoolRadiusStep ->
  m [DriverPoolResult]
calculateDriverPool pickup merchantId variant fareProductType radiusStep = do
  let pickupLatLong = getCoordinates pickup
  radius <- getRadius
  mbDriverPositionInfoExpiry <- asks (.driverPoolCfg.driverPositionInfoExpiry)
  nearestDriversResult <-
    measuringDurationToLog INFO "calculateDriverPool" $
      QP.getNearestDrivers
        pickupLatLong
        radius
        merchantId
        variant
        fareProductType
        mbDriverPositionInfoExpiry
  case nearestDriversResult of
    [] -> pure []
    (a : xs) -> do
      approxDriverPool' <- buildDriverPoolResults merchantId pickupLatLong (a :| xs)
      filterOutDriversWithDistanceAboveThreshold radius approxDriverPool'
  where
    getRadius = do
      minRadius <-
        QTConf.findValueByMerchantIdAndKey merchantId (STConf.ConfigKey "min_radius")
          >>= maybe
            (fromIntegral <$> asks (.driverPoolCfg.defaultRadiusOfSearch))
            radiusFromTransporterConfig
      maxRadius <-
        QTConf.findValueByMerchantIdAndKey merchantId (STConf.ConfigKey "max_radius")
          >>= maybe
            (fromIntegral <$> asks (.driverPoolCfg.defaultRadiusOfSearch))
            radiusFromTransporterConfig
      radiusStepSize <-
        QTConf.findValueByMerchantIdAndKey merchantId (STConf.ConfigKey "radius_step_size")
          >>= maybe
            (fromIntegral <$> asks (.driverPoolCfg.defaultRadiusOfSearch))
            radiusFromTransporterConfig
      return $ min (minRadius + radiusStepSize * radiusStep) maxRadius

    radiusFromTransporterConfig conf =
      fromMaybeM (InternalError "Value is not a number.")
        . readMaybe
        . toString
        $ conf.value

buildDriverPoolResults ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m
  ) =>
  Id DM.Merchant ->
  LatLong ->
  NonEmpty QP.NearestDriversResult ->
  m (NonEmpty DriverPoolResult)
buildDriverPoolResults orgId pickup ndResults = do
  distDurs <-
    Maps.getDistances orgId $
      Maps.GetDistancesReq
        { origins = ndResults,
          destinations = pickup :| [],
          travelMode = Just Maps.CAR
        }
  return $ mkDriverPoolResult <$> distDurs
  where
    mkDriverPoolResult distDur = do
      let QP.NearestDriversResult {..} = distDur.origin
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
