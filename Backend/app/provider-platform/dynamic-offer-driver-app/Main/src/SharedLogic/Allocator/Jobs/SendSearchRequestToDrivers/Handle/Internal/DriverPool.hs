{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool
  ( isBatchNumExceedLimit,
    incrementBatchNum,
    getPoolBatchNum,
    module Reexport,
    PrepareDriverPoolBatchEntity (..),
    incrementDriverRequestCount,
    previouslyAttemptedDrivers,
    checkRequestCount,
    isBookAny,
    makeTaggedDriverPool,
    ensurePoolingLogicVersion,
    splitSilentDriversAndSortWithDistance,
    previouslyAttemptedDriversKey,
  )
where

import qualified Control.Monad as CM
import Data.Aeson as A
import Data.Aeson.Types as A
import qualified Data.Hashable as DH
import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import qualified Domain.Types as DVST
import qualified Domain.Types.Common as DriverInfo
import Domain.Types.DriverPoolConfig
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Driver)
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.DatastoreLatencyCalculator
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config as Reexport
import qualified SharedLogic.DriverIdleTime as DriverIdleTime
import SharedLogic.DriverPool
import SharedLogic.DriverPool.DriverPoolData (checkRequestCount)
import qualified SharedLogic.DriverPool.DriverPoolData as DPD
import Storage.Beam.Yudhishthira ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.SearchRequest as QSR
import Tools.DynamicLogic

isBatchNumExceedLimit ::
  ( CacheFlow m r
  ) =>
  DriverPoolConfig ->
  Id DST.SearchTry ->
  m Bool
isBatchNumExceedLimit driverPoolConfig searchTryId = do
  let maxNumberOfBatches = driverPoolConfig.maxNumberOfBatches
  currentBatchNum <- getPoolBatchNum searchTryId
  return $ currentBatchNum >= maxNumberOfBatches

previouslyAttemptedDriversKey :: Id DST.SearchTry -> Maybe Bool -> Text
previouslyAttemptedDriversKey searchTryId consideOnRideDrivers = do
  -- v2 shape: [(Id Driver, ServiceTierType)] tuples. Old shape was [DriverPoolWithActualDistResult].
  -- Key bumped so deploys don't read stale-shape entries (which would fail JSON decode).
  case consideOnRideDrivers of
    Just consideOnRideDrivers' -> "Driver-Offer:PreviouslyAttemptedDrivers:v2:SearchTryId-" <> searchTryId.getId <> ":consideOnRideDrivers-" <> show consideOnRideDrivers'
    Nothing -> "Driver-Offer:PreviouslyAttemptedDrivers:v2:SearchTryId-" <> searchTryId.getId

splitSilentDriversAndSortWithDistance :: [DriverPoolWithActualDistResult] -> [DriverPoolWithActualDistResult]
splitSilentDriversAndSortWithDistance drivers = do
  let (silentDrivers, activeDrivers) = bimap (sortOn (.actualDistanceToPickup)) (sortOn (.actualDistanceToPickup)) $ DL.partition ((== Just DriverInfo.SILENT) . (.driverPoolResult.mode)) drivers
  activeDrivers <> silentDrivers

previouslyAttemptedDrivers ::
  ( Redis.HedisFlow m r
  ) =>
  Id DST.SearchTry ->
  Maybe Bool ->
  m [(Id Driver, DVST.ServiceTierType)]
previouslyAttemptedDrivers searchTryId consideOnRideDrivers = do
  Redis.withCrossAppRedis $
    Redis.safeGet (previouslyAttemptedDriversKey searchTryId consideOnRideDrivers)
      >>= maybe whenFoundNothing whenFoundSomething
  where
    whenFoundNothing = do
      logWarning "Unexpected empty driver pool batch cache."
      return []
    whenFoundSomething = \case
      [] -> do
        logWarning "Unexpected empty driver pool batch."
        return []
      a -> return a

-- | Deterministic 1-100 toss derived from the search request id, used for POOLING
-- version selection. All selections for one search request must land on the same
-- rollout even when they race (normal vs on-ride pool within a batch, overlapping
-- batch jobs reading a not-yet-persisted version), so the toss cannot be random per call.
poolingLogicVersionToss :: Id DSR.SearchRequest -> Int
poolingLogicVersionToss searchReqId = (DH.hash searchReqId.getId `mod` 100) + 1

-- | Resolve the POOLING logic version for a search request exactly once and persist it.
-- Called before each batch execution so that every consumer in that execution (normal and
-- on-ride pool selection, search_request_for_driver rows) and every later batch or search
-- try of the same search request sees the same version.
ensurePoolingLogicVersion ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  DSR.SearchRequest ->
  m DSR.SearchRequest
ensurePoolingLogicVersion searchReq
  | isJust searchReq.poolingLogicVersion = pure searchReq
  | otherwise = do
    transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = searchReq.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigDoesNotExist searchReq.merchantOperatingCityId.getId)
    localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
    mbVersion <- selectAppDynamicLogicVersion (cast searchReq.merchantOperatingCityId) LYT.POOLING localTime (Just $ poolingLogicVersionToss searchReq.id)
    whenJust mbVersion $ \_ -> QSR.updatePoolingLogicVersion mbVersion searchReq.id
    pure $ searchReq {DSR.poolingLogicVersion = mbVersion}

makeTaggedDriverPool ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r
  ) =>
  Id MerchantOperatingCity ->
  Seconds ->
  DSR.SearchRequest ->
  [DriverPoolWithActualDistResult] ->
  Int ->
  Bool ->
  Maybe [LYT.TagNameValue] ->
  Maybe Int ->
  PoolBatchNum ->
  DriverPoolConfig ->
  Id DST.SearchTry ->
  m (Maybe Int, [DriverPoolWithActualDistResult])
makeTaggedDriverPool mOCityId timeDiffFromUtc searchReq onlyNewDrivers batchSize isOnRidePool customerNammaTags mbPoolingLogicVersion batchNum driverPoolCfg searchTryId = do
  localTime <- getLocalCurrentTime timeDiffFromUtc
  (allLogics, mbVersion) <- getAppDynamicLogic (cast mOCityId) LYT.POOLING localTime mbPoolingLogicVersion (Just $ poolingLogicVersionToss searchReq.id)
  let onlyNewDriversWithCustomerInfo = map updateDriverPoolWithActualDistResult onlyNewDrivers
  -- Enrich drivers with their per-driver SRD sliding-window counters and idle time so the POOLING
  -- dynamic-logic rules can reference them. Batched: pipelined cross-slot MGETs for the whole batch
  -- (counters + idle) instead of a Redis read pass per driver.
  enrichedDrivers <- withTimeAPI "driverPooling" "enrichingDriversWithRealTimeData" $ do
    let personIds = map (\d -> cast d.driverPoolResult.driverId) onlyNewDriversWithCustomerInfo
    countersMap <- getSrdStatsCountersBulk driverPoolCfg.srdCountersBulkChunkSize personIds
    idleMap <- DriverIdleTime.getIdleTimeSecondsBulk driverPoolCfg.idleBulkChunkSize personIds
    return $
      map
        ( \driver ->
            let personId = cast driver.driverPoolResult.driverId
             in driver {searchReqDriverStatsCounters = Map.lookup personId countersMap, idleTimeSeconds = Map.lookup personId idleMap}
        )
        onlyNewDriversWithCustomerInfo
  let taggedDriverPoolInput = TaggedDriverPoolInput {drivers = enrichedDrivers, needOnRideDrivers = isOnRidePool, batchNum}
  logInfo $
    "DriverPreference pooling input: customerNammaTags=" <> show customerNammaTags
      <> " | drivers=["
      <> mconcat
        ( map
            ( \d ->
                "driverId=" <> show d.driverPoolResult.driverId
                  <> " gender="
                  <> show d.driverPoolResult.driverGender
                  <> " customerTags="
                  <> show d.driverPoolResult.customerTags
                  <> "; "
            )
            onlyNewDriversWithCustomerInfo
        )
      <> "]"
  resp <- withTimeAPI "driverPooling" "runLogics" $ LYDL.runLogicsWithDebugLog LYDL.Driver (cast mOCityId) LYT.POOLING (Just searchReq.transactionId) allLogics taggedDriverPoolInput
  sortedPool' <-
    case (A.fromJSON resp.result :: Result TaggedDriverPoolInput) of
      A.Success sortedPoolData -> pure sortedPoolData.drivers
      A.Error err -> do
        logError $ "Error in parsing sortedPoolData - " <> show err
        pure enrichedDrivers
  sortedPool <-
    filterM
      ( \driverPoolResult -> do
          now <- getCurrentTime
          let valueToPut = addUTCTime (fromIntegral driverPoolCfg.singleBatchProcessTime) now
          let fromScore = addUTCTime (-1 * (fromIntegral driverPoolCfg.singleBatchProcessTime)) now
          fork "removeExpiredSearchRequestInfoFromCache" $ removeExpiredSearchRequestInfoFromCache driverPoolCfg.merchantId driverPoolResult.driverPoolResult.driverId
          isLessThenNParallelRequests searchReq.id driverPoolCfg.merchantId driverPoolResult.driverPoolResult.driverId valueToPut driverPoolCfg.maxParallelSearchRequests fromScore
      )
      sortedPool'

  pushTaggedPoolToKafka sortedPool
  return (mbVersion, take batchSize sortedPool)
  where
    updateDriverPoolWithActualDistResult DriverPoolWithActualDistResult {..} =
      DriverPoolWithActualDistResult {driverPoolResult = updateDriverPoolResult driverPoolResult, searchTags = Just $ maybe A.emptyObject LYTU.convertTags searchReq.searchTags, tripDistance = searchReq.estimatedDistance, ..}

    updateDriverPoolResult DriverPoolResult {..} =
      DriverPoolResult {customerTags = Just $ maybe A.emptyObject LYTU.convertTags customerNammaTags, ..}

    pushTaggedPoolToKafka taggedPool = do
      pushToKafka
        ( SearchTryBatchPoolData
            { searchTryId = searchTryId.getId,
              driverPoolData = taggedPool,
              filterStage = TaggedPool,
              batchNum = batchNum
            }
        )
        "search-try-driver-tagged-pool-batch"
        searchTryId.getId

poolBatchNumKey :: Id DST.SearchTry -> Text
poolBatchNumKey searchTryId = "Driver-Offer:Allocator:PoolBatchNum:SearchTryId-" <> searchTryId.getId

getPoolBatchNum :: (Redis.HedisFlow m r) => Id DST.SearchTry -> m PoolBatchNum
getPoolBatchNum searchTryId = do
  res <- Redis.withCrossAppRedis $ Redis.get (poolBatchNumKey searchTryId)
  case res of
    Just i -> return i
    Nothing -> do
      let expTime = 600
      Redis.withCrossAppRedis $ Redis.setExp (poolBatchNumKey searchTryId) (-1 :: Integer) expTime
      return (-1)

incrementBatchNum ::
  ( Redis.HedisFlow m r
  ) =>
  Id DST.SearchTry ->
  m ()
incrementBatchNum searchTryId = do
  res <- Redis.withCrossAppRedis $ Redis.incr (poolBatchNumKey searchTryId)
  logInfo $ "Increment batch num to " <> show res <> "."
  return ()

isBookAny :: [DVST.ServiceTierType] -> Bool
isBookAny vehicleServiceTiers = length vehicleServiceTiers > 1

incrementDriverRequestCount :: (Redis.HedisFlow m r) => [DriverPoolWithActualDistResult] -> Id DST.SearchTry -> m ()
incrementDriverRequestCount finalPoolBatch searchTryId = do
  CM.mapM_
    ( \dpr ->
        Redis.withCrossAppRedis do
          void $ Redis.incr (DPD.driverRequestCountKey searchTryId dpr.driverPoolResult.driverId dpr.driverPoolResult.serviceTier)
          Redis.expire (DPD.driverRequestCountKey searchTryId dpr.driverPoolResult.driverId dpr.driverPoolResult.serviceTier) 7200
    )
    finalPoolBatch

data PrepareDriverPoolBatchEntity = PrepareDriverPoolBatchEntity
  { currentDriverPoolBatch :: [DriverPoolWithActualDistResult],
    poolType :: PoolType,
    nextScheduleTime :: Maybe Seconds,
    currentDriverPoolBatchOnRide :: [DriverPoolWithActualDistResult]
  }
