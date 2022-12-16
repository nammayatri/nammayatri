module Domain.Action.Allocation.Internal.DriverPool
  ( prepareDriverPoolBatches,
    getNextDriverPoolBatch,
    cleanupDriverPoolBatches,
    incrementPoolRadiusStep,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Data.List.Extra as List
import qualified Domain.Types.Booking as SRB
import Environment (Flow)
import SharedLogic.DriverPool (calculateDriverPool)
import SharedLogic.DriverPool.Config as Reexport
import SharedLogic.DriverPool.Types as Reexport
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Booking as QBooking
import Tools.Error
import Tools.Maps as Google
import Tools.Metrics

driverPoolKey :: Id SRB.Booking -> Text
driverPoolKey bookingId = "DriverPool:BookingId-" <> bookingId.getId

driverPoolBatchKey :: Id SRB.Booking -> PoolRadiusStep -> PoolBatchNum -> Text
driverPoolBatchKey bookingId radiusStep batchNum = driverPoolKey bookingId <> ":RadiusStep-" <> show radiusStep <> ":BatchNum-" <> show batchNum

prepareDriverPoolBatches ::
  ( EncFlow m r,
    HasCacheConfig r,
    CoreMetrics m,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasDriverPoolConfig r
  ) =>
  Id SRB.Booking ->
  m ()
prepareDriverPoolBatches bookingId = do
  radiusStep <- getPoolRadiusStep bookingId
  driverPool <- calcDriverPool radiusStep
  let sortedDriverPool = sortFunction driverPool
  driverPoolBatches <- splitToBatches sortedDriverPool
  cacheBatches radiusStep driverPoolBatches
  where
    calcDriverPool radiusStep = do
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
    cacheBatches radiusStep =
      void . flip foldlM 0 \i batch -> do
        Redis.setExp (driverPoolBatchKey bookingId i radiusStep) batch (60 * 10)
        return (i + 1)
    sortFunction driverPool =
      driverPool -- it's random order by default

getDriverPoolBatch ::
  ( Redis.HedisFlow m r
  ) =>
  Id SRB.Booking ->
  PoolRadiusStep ->
  PoolBatchNum ->
  m [DriverPoolResult]
getDriverPoolBatch bookingId radiusStep batchNum = do
  Redis.get (driverPoolBatchKey bookingId batchNum radiusStep)
    >>= maybe whenFoundNothing whenFoundSomething
  where
    whenFoundNothing = return []
    whenFoundSomething = \case
      [] -> do
        logWarning "Unexpected empty driver pool batch."
        return []
      a -> return a

poolBatchNumKey :: Id SRB.Booking -> Text
poolBatchNumKey bookingId = "Allocator:PoolBatchNum:BookingId-" <> bookingId.getId

poolRadiusStepKey :: Id SRB.Booking -> Text
poolRadiusStepKey bookingId = "Allocator:PoolRadiusStep:BookingId-" <> bookingId.getId

cleanupDriverPoolBatches ::
  ( Redis.HedisFlow m r
  ) =>
  Id SRB.Booking ->
  m ()
cleanupDriverPoolBatches bookingId = do
  Redis.delByPattern (driverPoolKey bookingId <> "*")
  Redis.del (poolRadiusStepKey bookingId)
  Redis.del (poolBatchNumKey bookingId)

getNextDriverPoolBatch :: Id SRB.Booking -> Flow [DriverPoolResult]
getNextDriverPoolBatch bookingId = do
  batchNum <- getPoolBatchNum
  radiusStep <- getPoolRadiusStep bookingId
  getDriverPoolBatch bookingId radiusStep batchNum
  where
    getPoolBatchNum :: (Redis.HedisFlow m r) => m PoolBatchNum
    getPoolBatchNum = do
      res <- Redis.get (poolBatchNumKey bookingId)
      res' <- case res of
        Just i -> return i
        Nothing -> do
          let expTime = 600
          Redis.setExp (poolBatchNumKey bookingId) (0 :: Integer) expTime
          return 0
      void $ Redis.incr (poolBatchNumKey bookingId)
      return res'

getPoolRadiusStep :: (Redis.HedisFlow m r) => Id SRB.Booking -> m PoolRadiusStep
getPoolRadiusStep bookingId = do
  res <- Redis.get (poolRadiusStepKey bookingId)
  case res of
    Just i -> return i
    Nothing -> do
      let expTime = 600
      Redis.setExp (poolRadiusStepKey bookingId) (0 :: Integer) expTime
      return 0

incrementPoolRadiusStep ::
  ( EncFlow m r,
    HasCacheConfig r,
    CoreMetrics m,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasDriverPoolConfig r
  ) =>
  Id SRB.Booking ->
  m ()
incrementPoolRadiusStep bookingId = do
  Redis.del (poolBatchNumKey bookingId)
  void $ Redis.incr (poolRadiusStepKey bookingId)
