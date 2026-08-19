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
    isBatchChainSuperseded,
  )
where

import qualified Control.Monad as CM
import Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types as A
import qualified Data.Hashable as DH
import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Types as DVST
import qualified Domain.Types.Common as DriverInfo
import Domain.Types.DriverPoolConfig
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Driver)
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Lib.Utils (pushToKafka)
import qualified Kernel.External.Maps as EMaps
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
import qualified SharedLogic.DriverPool.AreaPreference as AreaPref
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

-- | Per-driver preference-match checks for a search. Each entry is one independently
-- pluggable dimension over the driver's own self-selected preferences.
mkDriverPreferenceChecks :: DSR.SearchRequest -> DriverPoolWithActualDistResult -> [PreferenceCheck]
mkDriverPreferenceChecks searchReq driver =
  [rideDistanceCheck, pickupRadiusCheck, petModeCheck, areaCheck]
  where
    dpr = driver.driverPoolResult
    rideDistanceCheck =
      binaryCheck
        (isJust dpr.minRideDistance || isJust dpr.maxRideDistance)
        ( case driver.tripDistance of
            Nothing -> True -- ride distance unknown yet, don't penalize
            Just rideDistance -> maybe True (rideDistance >=) dpr.minRideDistance && maybe True (rideDistance <=) dpr.maxRideDistance
        )
    pickupRadiusCheck =
      binaryCheck
        (isJust dpr.maxPickupDistance)
        (maybe True (driver.actualDistanceToPickup <=) dpr.maxPickupDistance)
    petModeCheck =
      -- Only relevant when this search is itself a pet ride; an ordinary ride never
      -- counts against (or for) a driver's pet-mode setting.
      binaryCheck searchReq.isPetRide dpr.isPetModeEnabled
    -- Area preference is stored as tags on driverTags (see
    -- SharedLogic.DriverPool.AreaPreference), not a dedicated typed field, so this
    -- reads the raw tag object directly. Matched against the ride's DROP location
    -- only -- pickup is deliberately not considered (product decision).
    areaCheck = case dpr.driverTags of
      A.Object tagsObj
        | Just radiusValue <- lookupTagValue AreaPref.areaPreferenceRadiusTagName tagsObj,
          Just (center, radius) <- AreaPref.parseRadiusTagValue radiusValue ->
          case EMaps.getCoordinates <$> searchReq.toLocation of
            Nothing -> notApplicable
            Just dropPoint -> binaryCheck True (AreaPref.matchesRadius center radius dropPoint)
        | hasAnyCellTag tagsObj ->
          case searchReq.toLocGeohash of
            Nothing -> notApplicable
            Just dropGeohash -> binaryCheck True (KM.member (A.fromText (AreaPref.areaPreferenceCellTagName dropGeohash)) tagsObj)
      _ -> notApplicable
    hasAnyCellTag tagsObj = any (T.isPrefixOf AreaPref.areaPreferenceCellTagPrefix . A.toText) (KM.keys tagsObj)
    -- convertTags turns an '&'-joined tag value into a JSON array of strings
    -- (one element per '&' part), so the radius tag's value round-trips as
    -- ["lat","lon","radiusMeters"] rather than a single string.
    lookupTagValue name tagsObj = do
      v <- KM.lookup (A.fromText name) tagsObj
      case v of
        A.Array arr -> do
          parts <- traverse (\case A.String t -> Just t; _ -> Nothing) (toList arr)
          pure (T.intercalate "&" parts)
        A.String t -> Just t
        _ -> Nothing

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
  updateVersionInSearchReq mbVersion
  -- A rollout toss happens here whenever mbPoolingLogicVersion is Nothing, so the version applied to
  -- this pool can differ from the one already pinned on the SearchRequest. That divergence used to be
  -- invisible and mislabelled scores with the wrong experiment arm; make it greppable.
  when (isJust mbPoolingLogicVersion && mbVersion /= mbPoolingLogicVersion) $
    logError $
      "POOLING version mismatch: ran " <> show mbVersion <> " but SearchRequest is pinned to "
        <> show mbPoolingLogicVersion
        <> " (searchTryId="
        <> searchTryId.getId
        <> ", isOnRidePool="
        <> show isOnRidePool
        <> ")"
  logInfo $ "POOLING version applied: " <> show mbVersion <> " batchNum=" <> show batchNum <> " isOnRidePool=" <> show isOnRidePool
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
             in driver
                  { searchReqDriverStatsCounters = Map.lookup personId countersMap,
                    idleTimeSeconds = Map.lookup personId idleMap,
                    preferenceMatchScore = computePreferenceMatchScore (mkDriverPreferenceChecks searchReq driver)
                  }
        )
        onlyNewDriversWithCustomerInfo
  -- Rejects accumulated by the earlier batches of this search try, so the POOLING ruleset can
  -- escalate (widen radius, grow the batch) instead of drip-feeding an unwilling pool.
  cumulativeRejectCount <- getSearchTryRejectCount searchTryId
  let taggedDriverPoolInput = TaggedDriverPoolInput {drivers = enrichedDrivers, needOnRideDrivers = isOnRidePool, batchNum, cumulativeRejectCount = Just cumulativeRejectCount}
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
                  <> " preferenceMatchScore="
                  <> show d.preferenceMatchScore
                  <> "; "
            )
            -- Log the enriched list, not the pre-enrichment one: the counters, idle
            -- time and preference score are exactly what this line exists to explain,
            -- and they are only present after enrichment.
            enrichedDrivers
        )
      <> "]"
  resp <- withTimeAPI "driverPooling" "runLogics" $ LYDL.runLogicsWithDebugLog LYDL.Driver (cast mOCityId) LYT.POOLING (Just searchReq.transactionId) allLogics taggedDriverPoolInput
  -- Stamp the version we actually ran onto every driver, after decoding, so it travels with the
  -- `score` that version produced. Callers must read the version from here rather than from
  -- SearchRequest.poolingLogicVersion: that field is a pre-pool snapshot and, when the version is
  -- chosen by rollout toss on this very call, it does not yet hold the version being applied.
  sortedPool' <-
    map (\d -> d {poolingLogicVersion = mbVersion}) <$> case (A.fromJSON resp.result :: Result TaggedDriverPoolInput) of
      A.Success sortedPoolData -> pure sortedPoolData.drivers
      A.Error err -> do
        logError $ "Error in parsing sortedPoolData - " <> show err
        pure enrichedDrivers
  -- Parallel-request admission, in two passes. `maxParallelSearchRequests` stays the absolute
  -- ceiling; `softMaxParallelSearchRequests` is only a preference, so we fill the batch from
  -- drivers below the soft cap first and dip into the soft..hard band solely when the batch
  -- would otherwise under-fill. Spreading offers this way matters because acceptance is
  -- capacity-bounded: a driver already holding several live offers rejects nearly all of them.
  -- Unset soft cap == hard cap, i.e. provably the previous single-pass behaviour.
  now <- getCurrentTime
  let valueToPut = addUTCTime (fromIntegral driverPoolCfg.singleBatchProcessTime) now
      fromScore = addUTCTime (-1 * (fromIntegral driverPoolCfg.singleBatchProcessTime)) now
      hardCap = driverPoolCfg.maxParallelSearchRequests
      softCap = min hardCap (fromMaybe hardCap driverPoolCfg.softMaxParallelSearchRequests)
      -- `isLessThenNParallelRequests` is a single atomic check-and-reserve (zAddIfPossible), so
      -- a failed soft attempt reserves nothing and the hard retry below can't double-count.
      tryReserve cap driverPoolResult =
        isLessThenNParallelRequests searchReq.id driverPoolCfg.merchantId driverPoolResult.driverPoolResult.driverId valueToPut cap fromScore
  softResults <- forM sortedPool' $ \driverPoolResult -> do
    fork "removeExpiredSearchRequestInfoFromCache" $ removeExpiredSearchRequestInfoFromCache driverPoolCfg.merchantId driverPoolResult.driverPoolResult.driverId
    (driverPoolResult,) <$> tryReserve softCap driverPoolResult
  let underSoft = [d | (d, True) <- softResults]
      overSoft = [d | (d, False) <- softResults] -- at/over the soft cap, possibly still under the hard one
  sortedPool <-
    if softCap >= hardCap || length underSoft >= batchSize
      then pure underSoft
      else do
        backfill <- filterM (tryReserve hardCap) overSoft
        logInfo $
          "SoftParallelBackfill: searchTryId=" <> searchTryId.getId
            <> " batchNum="
            <> show batchNum
            <> " softCap="
            <> show softCap
            <> " hardCap="
            <> show hardCap
            <> " underSoft="
            <> show (length underSoft)
            <> " batchSize="
            <> show batchSize
            <> " backfilled="
            <> show (length backfill)
        -- Ranking is preserved: soft-cap drivers first, backfill appended behind them.
        pure (underSoft <> backfill)

  pushTaggedPoolToKafka sortedPool
  return (mbVersion, take batchSize sortedPool)
  where
    updateVersionInSearchReq mbVersion =
      when (isNothing searchReq.poolingLogicVersion && isJust mbVersion) $
        QSR.updatePoolingLogicVersion mbVersion searchReq.id

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

-- | Whether this job has been superseded as the owner of the search try's batch chain.
--
-- Exactly one chain may be live. An early batch advance (see the Reject branch of respondQuote)
-- bumps the search try's epoch and enqueues an immediate job, orphaning the job that was already
-- scheduled; the orphan reaches this check with a stale epoch and must stop *without*
-- rescheduling, or both chains would keep sending batches forever.
--
-- The comparison is not atomic with the bump, so an orphan that reads the epoch in the instant
-- before it moves can still send one batch. That is bounded and self-healing: its very next tick
-- sees the newer epoch and terminates. Preferred over a lock, which could stall the only live
-- chain if it were ever held across a tick.
--
-- No-op unless early advance is enabled for the merchant: with no advances the epoch stays 0 and
-- every job owns the chain.
isBatchChainSuperseded ::
  ( Redis.HedisFlow m r,
    Log m
  ) =>
  DriverPoolConfig ->
  Id DST.SearchTry ->
  Maybe Int ->
  m Bool
isBatchChainSuperseded driverPoolCfg searchTryId mbJobEpoch
  | not (fromMaybe False driverPoolCfg.enableEarlyBatchAdvanceOnFullReject) = pure False
  | otherwise = do
    currentEpoch <- getBatchEpoch searchTryId
    let jobEpoch = fromMaybe 0 mbJobEpoch
    if jobEpoch < currentEpoch
      then do
        logInfo $
          "BatchChainSuperseded: searchTryId=" <> searchTryId.getId
            <> " jobEpoch="
            <> show jobEpoch
            <> " currentEpoch="
            <> show currentEpoch
        pure True
      else pure False

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
