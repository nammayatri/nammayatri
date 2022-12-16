module Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool
  ( getDriverPoolBatch,
    prepareDriverPoolBatches,
    cleanupDriverPoolBatches,
    incrementPoolRadiusStep,
    intelligentPoolSelection,
    getNextDriverPoolBatch,
    randomizeAndLimitSelection,
    module Reexport,
  )
where

import Beckn.Storage.Esqueleto (EsqDBReplicaFlow)
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Id
import qualified Beckn.Types.SlidingWindowCounters as SWC
import Beckn.Utils.Common
import qualified Data.List.Extra as List
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Domain.Types.SearchRequest as DSR
import Environment (Flow)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config as Reexport
import SharedLogic.DriverPool
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import System.Random
import Tools.Maps as Maps
import Tools.Metrics

driverPoolKey :: Id DSR.SearchRequest -> Text
driverPoolKey searchReqId = "DriverPool:SearchReqId-" <> searchReqId.getId

driverPoolBatchKey :: Id DSR.SearchRequest -> PoolRadiusStep -> PoolBatchNum -> Text
driverPoolBatchKey searchReqId radiusStep batchNum = driverPoolKey searchReqId <> ":RadiusStep-" <> show radiusStep <> ":BatchNum-" <> show batchNum

prepareDriverPoolBatches ::
  ( EncFlow m r,
    HasCacheConfig r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasDriverPoolConfig r,
    HasDriverPoolBatchesConfig r,
    HasField "acceptanceWindowOptions" r SWC.SlidingWindowOptions
  ) =>
  DSR.SearchRequest ->
  m ()
prepareDriverPoolBatches searchReq = do
  radiusStep <- getPoolRadiusStep searchReq.id
  driverPool <- calcDriverPool radiusStep
  sortedDriverPool <- sortFunction driverPool
  driverPoolBatches <- splitToBatches sortedDriverPool
  cacheBatches radiusStep driverPoolBatches
  where
    calcDriverPool radiusStep = do
      let vehicleVariant = searchReq.vehicleVariant
          merchantId = searchReq.providerId
      let pickupLoc = searchReq.fromLocation
      let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
      calculateDriverPoolWithActualDist (Just vehicleVariant) pickupLatLong merchantId True radiusStep
    splitToBatches driverPool = do
      batchSize <- asks (.driverPoolBatchesCfg.driverBatchSize)
      return $ List.chunksOf batchSize driverPool
    cacheBatches radiusStep =
      void . flip foldlM 0 \i batch -> do
        Redis.setExp (driverPoolBatchKey searchReq.id i radiusStep) batch (60 * 10)
        return (i + 1)
    sortFunction driverPool = do
      sortingType <- asks (.driverPoolBatchesCfg.poolSortingType)
      case sortingType of
        ByAcceptanceRatio -> intelligentPoolSelection driverPool
        ByRandom -> randomizeAndLimitSelection driverPool

withWindowOptions ::
  ( Redis.HedisFlow m r,
    HasField "acceptanceWindowOptions" r SWC.SlidingWindowOptions,
    MonadFlow m
  ) =>
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withWindowOptions fn = do
  asks (.acceptanceWindowOptions) >>= fn

intelligentPoolSelection ::
  ( Redis.HedisFlow m r,
    HasField "acceptanceWindowOptions" r SWC.SlidingWindowOptions,
    MonadFlow m
  ) =>
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
intelligentPoolSelection dp =
  withWindowOptions $
    \wo ->
      map snd
        . sortOn (Down . fst)
        <$> ( (\poolWithRatio -> logInfo ("Drivers in Pool with acceptance ratios " <> show poolWithRatio) $> poolWithRatio)
                =<< mapM (\dPoolRes -> (,dPoolRes) <$> getLatestAcceptanceRatio dPoolRes.driverPoolResult.driverId wo) dp
            )

randomizeAndLimitSelection ::
  (MonadFlow m) =>
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
randomizeAndLimitSelection driverPool = do
  let poolLen = length driverPool
      startIdx = 0
      endIdx = poolLen - 1
  randomNumList <- getRandomNumberList startIdx endIdx
  return $ fmap (driverPool !!) randomNumList

--TODO: Create proper module to randomize things
getRandomNumberList :: (L.MonadFlow m) => Int -> Int -> m [Int]
getRandomNumberList start end = do
  n <- round <$> L.runIO getPOSIXTime
  let pureGen = mkStdGen n
  return $ toList $ nextNumber pureGen Set.empty
  where
    nextNumber :: RandomGen g => g -> Set.Set Int -> Set.Set Int
    nextNumber gen acc =
      if Set.size acc == end - start + 1
        then acc
        else
          let (n, gen') = randomR (start, end) gen
           in nextNumber gen' (Set.union (Set.singleton n) acc)

getDriverPoolBatch ::
  ( Redis.HedisFlow m r
  ) =>
  Id DSR.SearchRequest ->
  PoolRadiusStep ->
  PoolBatchNum ->
  m [DriverPoolWithActualDistResult]
getDriverPoolBatch searchReqId radiusStep batchNum = do
  Redis.get (driverPoolBatchKey searchReqId batchNum radiusStep)
    >>= maybe whenFoundNothing whenFoundSomething
  where
    whenFoundNothing = return []
    whenFoundSomething = \case
      [] -> do
        logWarning "Unexpected empty driver pool batch."
        return []
      a -> return a

poolBatchNumKey :: Id DSR.SearchRequest -> Text
poolBatchNumKey searchReqId = "Allocator:PoolBatchNum:SearchReqId-" <> searchReqId.getId

poolRadiusStepKey :: Id DSR.SearchRequest -> Text
poolRadiusStepKey searchReqId = "Allocator:PoolRadiusStep:SearchReqId-" <> searchReqId.getId

cleanupDriverPoolBatches ::
  ( Redis.HedisFlow m r
  ) =>
  Id DSR.SearchRequest ->
  m ()
cleanupDriverPoolBatches searchReqId = do
  Redis.delByPattern (driverPoolKey searchReqId <> "*")
  Redis.del (poolRadiusStepKey searchReqId)
  Redis.del (poolBatchNumKey searchReqId)

getNextDriverPoolBatch :: Id DSR.SearchRequest -> Flow [DriverPoolWithActualDistResult]
getNextDriverPoolBatch searchReqId = do
  batchNum <- getPoolBatchNum
  radiusStep <- getPoolRadiusStep searchReqId
  getDriverPoolBatch searchReqId radiusStep batchNum
  where
    getPoolBatchNum :: (Redis.HedisFlow m r) => m PoolBatchNum
    getPoolBatchNum = do
      res <- Redis.get (poolBatchNumKey searchReqId)
      res' <- case res of
        Just i -> return i
        Nothing -> do
          let expTime = 600
          Redis.setExp (poolBatchNumKey searchReqId) (0 :: Integer) expTime
          return 0
      void $ Redis.incr (poolBatchNumKey searchReqId)
      return res'

getPoolRadiusStep :: (Redis.HedisFlow m r) => Id DSR.SearchRequest -> m PoolRadiusStep
getPoolRadiusStep searchReqId = do
  res <- Redis.get (poolRadiusStepKey searchReqId)
  case res of
    Just i -> return i
    Nothing -> do
      let expTime = 600
      Redis.setExp (poolRadiusStepKey searchReqId) (0 :: Integer) expTime
      return 0

incrementPoolRadiusStep ::
  ( EncFlow m r,
    HasCacheConfig r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasDriverPoolConfig r
  ) =>
  DSR.SearchRequest ->
  m ()
incrementPoolRadiusStep searchReq = do
  Redis.del (poolBatchNumKey searchReq.id)
  void $ Redis.incr (poolRadiusStepKey searchReq.id)
