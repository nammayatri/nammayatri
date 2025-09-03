{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Analytics where

import qualified Data.Map as Map
import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverFlowStatus as SDFStatus
import qualified Storage.Clickhouse.DailyStats as CDaily
import qualified Storage.Clickhouse.DriverInformation as CDI
import qualified Storage.Clickhouse.DriverStats as CDS
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
import Tools.Error

data Period = Today | ThisWeek | ThisMonth | Yesterday | LastWeek | LastMonth
  deriving (Show, Eq)

data AllTimeMetric = TOTAL_RIDE_COUNT | RATING_SUM | CANCEL_COUNT | ACCEPTATION_COUNT | TOTAL_REQUEST_COUNT
  deriving (Show, Eq, Ord)

allTimeMetrics :: [AllTimeMetric]
allTimeMetrics = [TOTAL_RIDE_COUNT, RATING_SUM, CANCEL_COUNT, ACCEPTATION_COUNT, TOTAL_REQUEST_COUNT]

allTimeKeys :: Text -> [Text]
allTimeKeys operatorId = map (makeOperatorAnalyticsKey operatorId) allTimeMetrics

data AllTimeFallbackRes = AllTimeFallbackRes
  { totalRideCount :: Int,
    ratingSum :: Int,
    cancelCount :: Int,
    acceptationCount :: Int,
    totalRequestCount :: Int
  }
  deriving (Show, Eq)

convertToAllTimeFallbackRes :: [(AllTimeMetric, Int)] -> AllTimeFallbackRes
convertToAllTimeFallbackRes metricsList =
  let metricsMap = Map.fromList metricsList
      getMetricValue metric = Map.findWithDefault 0 metric metricsMap
   in AllTimeFallbackRes
        { totalRideCount = getMetricValue TOTAL_RIDE_COUNT,
          ratingSum = getMetricValue RATING_SUM,
          cancelCount = getMetricValue CANCEL_COUNT,
          acceptationCount = getMetricValue ACCEPTATION_COUNT,
          totalRequestCount = getMetricValue TOTAL_REQUEST_COUNT
        }

data PeriodMetric = TOTAL_APPLICATION_COUNT | DRIVER_ENABLED | GREATER_THAN_ONE_RIDE | GREATER_THAN_TEN_RIDE | GREATER_THAN_FIFTY_RIDE
  deriving (Show, Eq, Ord)

periodMetrics :: [PeriodMetric]
periodMetrics = [TOTAL_APPLICATION_COUNT, DRIVER_ENABLED, GREATER_THAN_ONE_RIDE, GREATER_THAN_TEN_RIDE, GREATER_THAN_FIFTY_RIDE]

periodKeys :: Text -> Period -> [Text]
periodKeys operatorId period = map (\metric -> makeOperatorPeriodicKey operatorId metric period) periodMetrics

data PeriodFallbackRes = PeriodFallbackRes
  { totalApplicationCount :: Int,
    driverEnabled :: Int,
    greaterThanOneRide :: Int,
    greaterThanTenRide :: Int,
    greaterThanFiftyRide :: Int
  }
  deriving (Show, Eq)

convertToPeriodFallbackRes :: [(PeriodMetric, Int)] -> PeriodFallbackRes
convertToPeriodFallbackRes metricsList =
  let metricsMap = Map.fromList metricsList
      getMetricValue metric = Map.findWithDefault 0 metric metricsMap
   in PeriodFallbackRes
        { totalApplicationCount = getMetricValue TOTAL_APPLICATION_COUNT,
          driverEnabled = getMetricValue DRIVER_ENABLED,
          greaterThanOneRide = getMetricValue GREATER_THAN_ONE_RIDE,
          greaterThanTenRide = getMetricValue GREATER_THAN_TEN_RIDE,
          greaterThanFiftyRide = getMetricValue GREATER_THAN_FIFTY_RIDE
        }

-- | Redis key functions for fleet analytics
makeOperatorKeyPrefix :: Text -> Text
makeOperatorKeyPrefix operatorId = "operator:" <> operatorId <> ":"

makeOperatorAnalyticsKey :: Text -> AllTimeMetric -> Text
makeOperatorAnalyticsKey operatorId metric = makeOperatorKeyPrefix operatorId <> show metric

makeOperatorPeriodicKey :: Text -> PeriodMetric -> Period -> Text
makeOperatorPeriodicKey operatorId metric period = makeOperatorKeyPrefix operatorId <> show metric <> ":" <> show period

-- | Find the operator ID for a driver by checking direct association or fleet association
findOperatorIdForDriver ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  m (Maybe Text)
findOperatorIdForDriver driverId = do
  -- First try to find direct driver-operator association
  mbDriverOperatorAssoc <- QDOA.findByDriverId driverId True
  case mbDriverOperatorAssoc of
    Just assoc -> pure (Just assoc.operatorId)
    Nothing -> do
      -- If no direct association, try to find through fleet
      mbFleetDriverAssoc <- QFDA.findByDriverId driverId True
      case mbFleetDriverAssoc of
        Just fleetAssoc -> do
          -- Found fleet association, now find the operator for this fleet
          mbFleetOperatorAssoc <- QFOA.findByFleetOwnerIdAndIsActive (Id fleetAssoc.fleetOwnerId) True
          case mbFleetOperatorAssoc of
            Just fleetOpAssoc -> pure (Just fleetOpAssoc.operatorId)
            Nothing -> pure Nothing
        Nothing -> pure Nothing

-- | Common function to ensure Redis keys exist by falling back to ClickHouse if needed
ensureRedisKeysExistForAllTime ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  Text ->
  Int ->
  m ()
ensureRedisKeysExistForAllTime operatorId targetKey incrValue = do
  mbValue <- Redis.get @Int targetKey
  let msg = "Key does not exist for operator alltime analytics key: " <> show targetKey <> ", operatorId: " <> show operatorId

  if isNothing mbValue
    then fork msg $ do
      logTagInfo "AllTimeAnalytics" $ "Key does not exist. Handling cache miss for operatorId: " <> show operatorId
      let allTimeKeysData = allTimeKeys operatorId
      void $ handleCacheMissForOperatorAnalyticsAllTime operatorId allTimeKeysData
      void $ Redis.incrby targetKey (fromIntegral incrValue)
      logTagInfo "AllTimeAnalytics" $ "Updated key after cache miss: key=" <> show targetKey <> ", operatorId=" <> show operatorId
    else do
      logTagInfo "AllTimeAnalytics" $ "Key already exists for operator analytics key: " <> show targetKey <> ", operatorId=" <> show operatorId
      void $ Redis.incrby targetKey (fromIntegral incrValue)

ensureRedisKeysExistForPeriod ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  (Text -> m Integer) ->
  Text ->
  Period ->
  m ()
ensureRedisKeysExistForPeriod operatorId redisOps targetKey period = do
  mbValue <- Redis.get @Int targetKey
  now <- getCurrentTime
  let msg = "Key does not exist for operator period analytics key: " <> show targetKey <> ", operatorId: " <> show operatorId

  if isNothing mbValue
    then fork msg $ do
      let (fromDay, toDay) = deriveFromAndToDay period now
          periodKeysData = periodKeys operatorId period
      logTagInfo "PeriodAnalytics" $ "Key does not exist. Handling cache miss for operatorId: " <> show operatorId
      void $ handleCacheMissForOperatorAnalyticsPeriod operatorId periodKeysData period fromDay toDay
      void $ redisOps targetKey
      logTagInfo "PeriodAnalytics" $ "Updated key after cache miss: key=" <> show targetKey <> ", operatorId=" <> show operatorId
    else do
      logTagInfo "PeriodAnalytics" $ "Key already exists for operator analytics key: " <> show targetKey <> ", operatorId=" <> show operatorId
      void $ redisOps targetKey

-- | Update the operator analytics cancel count for a driver
updateOperatorAnalyticsCancelCount ::
  ( MonadFlow m,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Id DP.Person ->
  m ()
updateOperatorAnalyticsCancelCount driverId = do
  -- Find the operator ID for this driver
  mbOperatorId <- findOperatorIdForDriver driverId
  when (isNothing mbOperatorId) $ logTagError "AnalyticsUpdateCancelCount" "No operator found for driver"
  whenJust mbOperatorId $ \operatorId -> do
    let cancelCountKey = makeOperatorAnalyticsKey operatorId CANCEL_COUNT
    -- Ensure Redis keys exist
    ensureRedisKeysExistForAllTime operatorId cancelCountKey 1

updateOperatorAnalyticsAcceptationAndTotalRequestCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Id DP.Person ->
  Maybe Int ->
  Maybe Int ->
  m ()
updateOperatorAnalyticsAcceptationAndTotalRequestCount driverId mbAcceptationCount mbTotalRequestCount = do
  mbOperatorId <- findOperatorIdForDriver driverId
  when (isNothing mbOperatorId) $ logTagError "AnalyticsUpdateAcceptationAndTotalRequestCount" "No operator found for driver"
  whenJust mbOperatorId $ \operatorId -> do
    when (isJust mbAcceptationCount) $ updateAcceptationCount operatorId
    when (isJust mbTotalRequestCount) $ updateTotalRequestCount operatorId
  where
    updateAcceptationCount operatorId = do
      let acceptationCountKey = makeOperatorAnalyticsKey operatorId ACCEPTATION_COUNT
      ensureRedisKeysExistForAllTime operatorId acceptationCountKey 1
    updateTotalRequestCount operatorId = do
      let totalRequestCountKey = makeOperatorAnalyticsKey operatorId TOTAL_REQUEST_COUNT
      ensureRedisKeysExistForAllTime operatorId totalRequestCountKey 1

-- | Update the operator analytics rating score for a driver
updateOperatorAnalyticsRatingScoreKey ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Id DP.Person ->
  Int ->
  m ()
updateOperatorAnalyticsRatingScoreKey driverId ratingValue = do
  mbOperatorId <- findOperatorIdForDriver driverId
  when (isNothing mbOperatorId) $ logTagError "AnalyticsUpdateRatingScoreKey" "No operator found for driver"
  whenJust mbOperatorId $ \operatorId -> do
    let ratingSumKey = makeOperatorAnalyticsKey operatorId RATING_SUM
    -- Ensure Redis keys exist
    ensureRedisKeysExistForAllTime operatorId ratingSumKey ratingValue

updateOperatorAnalyticsTotalRideCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Id DP.Person ->
  Int ->
  m ()
updateOperatorAnalyticsTotalRideCount driverId newTotalRides = do
  mbOperatorId <- findOperatorIdForDriver driverId
  when (isNothing mbOperatorId) $ logTagError "AnalyticsUpdateTotalRideCount" "No operator found for driver"
  whenJust mbOperatorId $ \operatorId -> do
    let totalRideCountKey = makeOperatorAnalyticsKey operatorId TOTAL_RIDE_COUNT
    -- Ensure Redis keys exist
    ensureRedisKeysExistForAllTime operatorId totalRideCountKey 1

    case newTotalRides of
      2 -> updatePeriodicMetrics operatorId GREATER_THAN_ONE_RIDE Redis.incr
      11 -> updatePeriodicMetrics operatorId GREATER_THAN_TEN_RIDE Redis.incr
      51 -> updatePeriodicMetrics operatorId GREATER_THAN_FIFTY_RIDE Redis.incr
      _ -> pure ()

updatePeriodicMetrics ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  PeriodMetric ->
  (Text -> m Integer) ->
  m ()
updatePeriodicMetrics operatorId metric redisOps = do
  let periods = [Today, ThisWeek, ThisMonth]
      updateForPeriod period = do
        let key = makeOperatorPeriodicKey operatorId metric period
        ensureRedisKeysExistForPeriod operatorId redisOps key period
  mapM_ updateForPeriod periods

incrementOperatorAnalyticsDriverEnabled ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Id DP.Person ->
  m ()
incrementOperatorAnalyticsDriverEnabled driverId = do
  mbOperatorId <- findOperatorIdForDriver driverId
  when (isNothing mbOperatorId) $ logTagError "AnalyticsUpdateDriverEnabled" "No operator found for driver"
  whenJust mbOperatorId $ \operatorId ->
    updatePeriodicMetrics operatorId DRIVER_ENABLED Redis.incr

decrementOperatorAnalyticsDriverEnabled ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Id DP.Person ->
  m ()
decrementOperatorAnalyticsDriverEnabled driverId = do
  mbOperatorId <- findOperatorIdForDriver driverId
  when (isNothing mbOperatorId) $ logTagError "AnalyticsUpdateDriverEnabled" "No operator found for driver"
  whenJust mbOperatorId $ \operatorId ->
    updatePeriodicMetrics operatorId DRIVER_ENABLED Redis.decr

incrementOperatorAnalyticsApplicationCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  m ()
incrementOperatorAnalyticsApplicationCount operatorId =
  updatePeriodicMetrics operatorId TOTAL_APPLICATION_COUNT Redis.incr

decrementOperatorAnalyticsApplicationCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  m ()
decrementOperatorAnalyticsApplicationCount operatorId =
  updatePeriodicMetrics operatorId TOTAL_APPLICATION_COUNT Redis.decr

-- | ClickHouse fallback function for fleet analytics
fallbackToClickHouseAndUpdateRedisForAllTime ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  [Text] ->
  m AllTimeFallbackRes
fallbackToClickHouseAndUpdateRedisForAllTime operatorId allTimeKeysData = do
  logTagInfo "Fallback to ClickHouse" "true"
  driverIds <- SDFStatus.getFleetDriverIdsAndDriverIdsByOperatorId operatorId
  driverStats <- CDS.sumRatingAndTotalRidesByDriverIds driverIds
  let tr = driverStats.totalRidesSum
      rs = fromMaybe 0 driverStats.totalRatingScoreSum
      cc = driverStats.cancelledCount
      ac = fromMaybe 0 driverStats.acceptationCountSum
      trc = fromMaybe 0 driverStats.totalRequestCountSum
  -- update redis (best-effort)
  mapM_ (uncurry Redis.set) (zip allTimeKeysData [tr, rs, cc, ac, trc])
  pure $ convertToAllTimeFallbackRes (zip allTimeMetrics [tr, rs, cc, ac, trc])

-- | Compute period dashboard analytics via ClickHouse for a given operator and time window
fallbackComputePeriodOperatorAnalytics ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  Day ->
  Day ->
  m PeriodFallbackRes
fallbackComputePeriodOperatorAnalytics operatorId fromDay toDay = do
  driverIds <- SDFStatus.getFleetDriverIdsAndDriverIdsByOperatorId operatorId
  appCount <- SDFStatus.getTotalFleetDriverAndDriverCountByOperatorIdInDateRange operatorId (UTCTime fromDay 0) (UTCTime toDay 86399)
  enabledCount <- CDI.getEnabledDriverCountByDriverIds driverIds (UTCTime fromDay 0) (UTCTime toDay 86399)
  gt1 <- CDaily.countDriversWithNumRidesGreaterThan1Between driverIds fromDay toDay
  gt10 <- CDaily.countDriversWithNumRidesGreaterThan10Between driverIds fromDay toDay
  gt50 <- CDaily.countDriversWithNumRidesGreaterThan50Between driverIds fromDay toDay
  pure PeriodFallbackRes {totalApplicationCount = appCount, driverEnabled = enabledCount, greaterThanOneRide = gt1, greaterThanTenRide = gt10, greaterThanFiftyRide = gt50}

-- | Fallback to ClickHouse for period analytics and update Redis keys for the period
fallbackToClickHouseAndUpdateRedisForPeriod ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  [Text] ->
  Period ->
  Day ->
  Day ->
  m PeriodFallbackRes
fallbackToClickHouseAndUpdateRedisForPeriod operatorId periodKeysData period fromDay toDay = do
  logTagInfo "Fallback to ClickHouse (period)" "true"
  res <- fallbackComputePeriodOperatorAnalytics operatorId fromDay toDay
  now <- getCurrentTime
  let expireTime = getPeriodExpireTime period now

  -- write back to Redis using setNx with expiration
  mapM_ (\(key, value) -> Redis.setExp key value expireTime) (zip periodKeysData [res.totalApplicationCount, res.driverEnabled, res.greaterThanOneRide, res.greaterThanTenRide, res.greaterThanFiftyRide])
  pure res

handleCacheMissForOperatorAnalyticsAllTime ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  [Text] ->
  m AllTimeFallbackRes
handleCacheMissForOperatorAnalyticsAllTime operatorId allTimeKeysData = do
  let inProgressKey = makeOperatorKeyPrefix operatorId <> "inProgressAllTime"
  inProgress <- Redis.get @Bool inProgressKey
  case inProgress of
    Just True -> do
      logTagInfo "OperatorAnalyticsAllTime" $ "inProgress key present for operatorId: " <> operatorId <> ". Waiting for it to clear."
      SDFStatus.waitUntilKeyGone inProgressKey
      allTimeKeysRes <- mapM (\key -> fromMaybe 0 <$> Redis.get @Int key) allTimeKeysData
      pure $ convertToAllTimeFallbackRes (zip allTimeMetrics allTimeKeysRes)
    _ -> do
      lockAcquired <- Redis.setNxExpire inProgressKey 30 True -- 30 seconds expiry
      if lockAcquired
        then do
          logTagInfo "OperatorAnalyticsAllTime" $ "Acquired inProgress lock for operatorId: " <> operatorId <> ". Running ClickHouse query."
          res <- try @_ @SomeException $ fallbackToClickHouseAndUpdateRedisForAllTime operatorId allTimeKeysData
          Redis.del inProgressKey
          case res of
            Left err -> do
              logTagError "OperatorAnalyticsAllTime" $ "Error during ClickHouse/Redis operation for operatorId: " <> operatorId <> ". Error: " <> show err
              throwError (InternalError $ "Failed to perform operation for operatorId: " <> operatorId)
            Right allTimeRes -> pure allTimeRes
        else do
          logTagInfo "OperatorAnalyticsAllTime" $ "inProgress lock already held for operatorId: " <> operatorId <> " (race detected in else). Waiting for it to clear."
          handleCacheMissForOperatorAnalyticsAllTime operatorId allTimeKeysData

handleCacheMissForOperatorAnalyticsPeriod ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  [Text] ->
  Period ->
  Day ->
  Day ->
  m PeriodFallbackRes
handleCacheMissForOperatorAnalyticsPeriod operatorId periodKeysData period fromDay toDay = do
  let inProgressKey = makeOperatorKeyPrefix operatorId <> "inProgressPeriod" <> show period
  inProgress <- Redis.get @Bool inProgressKey
  case inProgress of
    Just True -> do
      logTagInfo "OperatorAnalyticsPeriod" $ "inProgress key present for operatorId: " <> operatorId <> ". Waiting for it to clear."
      SDFStatus.waitUntilKeyGone inProgressKey
      periodKeysRes <- mapM (\key -> fromMaybe 0 <$> Redis.get @Int key) periodKeysData
      pure $ convertToPeriodFallbackRes (zip periodMetrics periodKeysRes)
    _ -> do
      lockAcquired <- Redis.setNxExpire inProgressKey 30 True -- 30 seconds expiry
      if lockAcquired
        then do
          logTagInfo "OperatorAnalyticsPeriod" $ "Acquired inProgress lock for operatorId: " <> operatorId <> ". Running ClickHouse query."
          res <- try @_ @SomeException $ fallbackToClickHouseAndUpdateRedisForPeriod operatorId periodKeysData period fromDay toDay
          Redis.del inProgressKey
          case res of
            Left err -> do
              logTagError "OperatorAnalyticsPeriod" $ "Error during ClickHouse/Redis operation for operatorId: " <> operatorId <> ". Error: " <> show err
              throwError (InternalError $ "Failed to perform operation for operatorId: " <> operatorId)
            Right periodRes -> pure periodRes
        else do
          logTagInfo "OperatorAnalyticsPeriod" $ "inProgress lock already held for operatorId: " <> operatorId <> " (race detected in else). Waiting for it to clear."
          handleCacheMissForOperatorAnalyticsPeriod operatorId periodKeysData period fromDay toDay

-- helper functions

getPeriodDefinitions :: Day -> [(Period, (Day, Day))]
getPeriodDefinitions today =
  let (y, m, _) = toGregorian today
      dow = calculateDayNumberOfWeek today
      thisWeekStart = addDays (negate dow) today
      firstOfMonth = fromGregorian y m 1
      yesterday = addDays (-1) today
      lastWeekStart = addDays (-7) thisWeekStart
      lastWeekEnd = addDays (-1) thisWeekStart
      lastMonthStart = addGregorianMonthsClip (-1) firstOfMonth
      lastMonthEnd = addDays (-1) firstOfMonth
   in [ (Today, (today, today)),
        (ThisWeek, (thisWeekStart, today)),
        (ThisMonth, (firstOfMonth, today)),
        (Yesterday, (yesterday, yesterday)),
        (LastWeek, (lastWeekStart, lastWeekEnd)),
        (LastMonth, (lastMonthStart, lastMonthEnd))
      ]

inferPeriod :: Day -> Day -> Day -> Maybe Period
inferPeriod now from to =
  let periodDefs = getPeriodDefinitions now
   in fst <$> find (\(_, (start, end)) -> from == start && to == end) periodDefs

getPeriodExpireTime :: Period -> UTCTime -> Int
getPeriodExpireTime period now =
  let today = utctDay now
      (y, m, d) = toGregorian today
      dow = calculateDayNumberOfWeek today

      endOfDay = UTCTime (fromGregorian y m d) 86400 -- 23:59:59
      endOfWeek = addUTCTime (fromIntegral ((6 - dow) * 86400 + 86399)) (UTCTime (fromGregorian y m d) 0)
      firstOfNextMonth = addGregorianMonthsClip 1 (fromGregorian y m 1)
      endOfMonth = addUTCTime (-1) (UTCTime firstOfNextMonth 0)

      expireAt = case period of
        Today -> endOfDay
        Yesterday -> endOfDay
        ThisWeek -> endOfWeek
        LastWeek -> endOfWeek
        ThisMonth -> endOfMonth
        LastMonth -> endOfMonth
   in round $ diffUTCTime expireAt now

calculateDayNumberOfWeek :: Day -> Integer
calculateDayNumberOfWeek = fromIntegral . (\d -> d - 1) . fromEnum . dayOfWeek

deriveFromAndToDay :: Period -> UTCTime -> (Day, Day)
deriveFromAndToDay period now =
  let today = utctDay now
      periodDefs = getPeriodDefinitions today
   in fromMaybe (today, today) $ lookup period periodDefs
