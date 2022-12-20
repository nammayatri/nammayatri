module Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool
  ( isBatchNumExceedLimit,
    cleanupDriverPoolBatches,
    getNextDriverPoolBatch,
    module Reexport,
  )
where

import Beckn.Storage.Esqueleto (EsqDBReplicaFlow)
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Id
import qualified Beckn.Types.SlidingWindowCounters as SWC
import Beckn.Utils.Common
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

isBatchNumExceedLimit ::
  ( Redis.HedisFlow m r,
    HasDriverPoolBatchesConfig r
  ) =>
  Id DSR.SearchRequest ->
  m Bool
isBatchNumExceedLimit searchReqId = do
  maxNumberOfBatches <- asks (.driverPoolBatchesCfg.maxNumberOfBatches)
  currentBatchNum <- getPoolBatchNum searchReqId
  return $ currentBatchNum >= maxNumberOfBatches

driverPoolKey :: Id DSR.SearchRequest -> Text
driverPoolKey searchReqId = "DriverPool:SearchReqId-" <> searchReqId.getId

driverPoolBatchKey :: Id DSR.SearchRequest -> PoolBatchNum -> Text
driverPoolBatchKey searchReqId batchNum = driverPoolKey searchReqId <> ":BatchNum-" <> show batchNum

prepareDriverPoolBatch ::
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
  PoolBatchNum ->
  m [DriverPoolWithActualDistResult]
prepareDriverPoolBatch searchReq batchNum = do
  previousBatchesDrivers <- getPreviousBatchesDrivers
  prepareDriverPoolBatch' previousBatchesDrivers
  where
    prepareDriverPoolBatch' previousBatchesDrivers = do
      radiusStep <- getPoolRadiusStep searchReq.id
      driverPool <- calcDriverPool radiusStep
      sortedDriverPool <- sortFunction driverPool
      let onlyNewDriversPool = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` previousBatchesDrivers) sortedDriverPool
      driverPoolBatch <- getBatch onlyNewDriversPool
      batchSize <- asks (.driverPoolBatchesCfg.driverBatchSize)
      if length driverPoolBatch < batchSize
        then do
          isAtMaxRadiusStep' <- isAtMaxRadiusStep radiusStep
          if isAtMaxRadiusStep'
            then do
              if null driverPoolBatch
                then return []
                else do
                  filledBatch <- fillBatch batchSize sortedDriverPool driverPoolBatch
                  cacheBatch filledBatch
                  return filledBatch
            else do
              incrementPoolRadiusStep searchReq.id
              prepareDriverPoolBatch' previousBatchesDrivers
        else do
          cacheBatch driverPoolBatch
          return driverPoolBatch

    calcDriverPool radiusStep = do
      let vehicleVariant = searchReq.vehicleVariant
          merchantId = searchReq.providerId
      let pickupLoc = searchReq.fromLocation
      let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
      calculateDriverPoolWithActualDist (Just vehicleVariant) pickupLatLong merchantId True radiusStep
    getBatch driverPool = do
      batchSize <- asks (.driverPoolBatchesCfg.driverBatchSize)
      return $ take batchSize driverPool
    fillBatch batchSize driverPool batch = do
      let batchDriverIds = batch <&> (.driverPoolResult.driverId)
      let driversToFillBatch = take (batchSize - length batch) $ filter (\dpr -> dpr.driverPoolResult.driverId `notElem` batchDriverIds) driverPool
      return $ batch <> driversToFillBatch
    cacheBatch batch = do
      Redis.setExp (driverPoolBatchKey searchReq.id batchNum) batch (60 * 10)
    sortFunction driverPool = do
      sortingType <- asks (.driverPoolBatchesCfg.poolSortingType)
      case sortingType of
        ByAcceptanceRatio -> intelligentPoolSelection driverPool
        ByRandom -> randomizeAndLimitSelection driverPool
    isAtMaxRadiusStep radiusStep = do
      minRadiusOfSearch <- fromIntegral @_ @Double <$> asks (.driverPoolCfg.minRadiusOfSearch)
      maxRadiusOfSearch <- fromIntegral @_ @Double <$> asks (.driverPoolCfg.maxRadiusOfSearch)
      radiusStepSize <- fromIntegral @_ @Double <$> asks (.driverPoolCfg.radiusStepSize)
      let maxRadiusStep = ceiling $ (maxRadiusOfSearch - minRadiusOfSearch) / radiusStepSize
      return $ maxRadiusStep <= radiusStep
    getPreviousBatchesDrivers = do
      batches <- forM [0 .. (batchNum - 1)] \num -> do
        getDriverPoolBatch searchReq.id num
      return $ (.driverPoolResult.driverId) <$> concat batches

getDriverPoolBatch ::
  ( Redis.HedisFlow m r
  ) =>
  Id DSR.SearchRequest ->
  PoolBatchNum ->
  m [DriverPoolWithActualDistResult]
getDriverPoolBatch searchReqId batchNum = do
  Redis.get (driverPoolBatchKey searchReqId batchNum)
    >>= maybe whenFoundNothing whenFoundSomething
  where
    whenFoundNothing = return []
    whenFoundSomething = \case
      [] -> do
        logWarning "Unexpected empty driver pool batch."
        return []
      a -> return a

intelligentPoolSelection ::
  ( Redis.HedisFlow m r,
    HasField "acceptanceWindowOptions" r SWC.SlidingWindowOptions,
    MonadFlow m
  ) =>
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
intelligentPoolSelection dp =
  map snd
    . sortOn (Down . fst)
    <$> ( (\poolWithRatio -> logInfo ("Drivers in Pool with acceptance ratios " <> show poolWithRatio) $> poolWithRatio)
            =<< mapM (\dPoolRes -> (,dPoolRes) <$> getLatestAcceptanceRatio dPoolRes.driverPoolResult.driverId) dp
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

getNextDriverPoolBatch :: DSR.SearchRequest -> Flow [DriverPoolWithActualDistResult]
getNextDriverPoolBatch searchReq = do
  batchNum <- getPoolBatchNum searchReq.id
  prepareDriverPoolBatch searchReq batchNum

getPoolBatchNum :: (Redis.HedisFlow m r) => Id DSR.SearchRequest -> m PoolBatchNum
getPoolBatchNum searchReqId = do
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
  Id DSR.SearchRequest ->
  m ()
incrementPoolRadiusStep searchReqId = do
  void $ Redis.incr (poolRadiusStepKey searchReqId)
