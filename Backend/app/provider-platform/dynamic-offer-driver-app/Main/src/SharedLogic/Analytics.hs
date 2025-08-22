{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Analytics where

import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.Person as DP
import EulerHS.Prelude
import Kernel.Prelude hiding (any, elem, map)
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

-- | Find the operator ID for a driver by checking direct association or fleet association
findOperatorIdForDriver ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  m Text
findOperatorIdForDriver driverId = do
  -- First try to find direct driver-operator association
  mbDriverOperatorAssoc <- QDOA.findByDriverId driverId True
  case mbDriverOperatorAssoc of
    Just assoc -> pure assoc.operatorId
    Nothing -> do
      -- If no direct association, try to find through fleet
      mbFleetDriverAssoc <- QFDA.findByDriverId driverId True
      case mbFleetDriverAssoc of
        Just fleetAssoc -> do
          -- Found fleet association, now find the operator for this fleet
          mbFleetOperatorAssoc <- QFOA.findByFleetOwnerIdAndIsActive (Id fleetAssoc.fleetOwnerId) True
          case mbFleetOperatorAssoc of
            Just fleetOpAssoc -> pure fleetOpAssoc.operatorId
            Nothing -> throwError $ InvalidRequest "Driver is associated with fleet but fleet has no operator"
        Nothing -> throwError $ InvalidRequest "Driver is not associated with any entity"

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
  m ()
ensureRedisKeysExistForAllTime operatorId targetKey = do
  mbValue <- Redis.get @Int targetKey

  when (isNothing mbValue) $ do
    let totalRideKey = makeOperatorAnalyticsKey operatorId "totalRideCount"
        ratingSumKey = makeOperatorAnalyticsKey operatorId "ratingSum"
        cancelCountKey = makeOperatorAnalyticsKey operatorId "cancelCount"
    (_, _, _) <- handleCacheMissForOperatorAnalyticsAllTime operatorId totalRideKey ratingSumKey cancelCountKey
    return ()

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

  when (isNothing mbValue) $ do
    let kAppCount = makeOperatorPeriodicKey operatorId "totalApplicationCount" (show period)
        kDriverEnabled = makeOperatorPeriodicKey operatorId "driverEnabled" (show period)
        kGt1 = makeOperatorPeriodicKey operatorId "greaterThanOneRide" (show period)
        kGt10 = makeOperatorPeriodicKey operatorId "greaterThanTenRide" (show period)
        kGt50 = makeOperatorAnalyticsKey operatorId "greaterThanFiftyRide"
        (fromDay, toDay) = deriveFromAndToDay period now
    (_, _, _, _, _) <- handleCacheMissForOperatorAnalyticsPeriod operatorId kAppCount kDriverEnabled kGt1 kGt10 kGt50 period fromDay toDay
    return ()

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
  operatorId <- findOperatorIdForDriver driverId
  let cancelCountKey = makeOperatorAnalyticsKey operatorId "cancelCount"
  -- Ensure Redis keys exist
  ensureRedisKeysExistForAllTime operatorId cancelCountKey

  -- Increment the cancel count
  void $ Redis.incr cancelCountKey

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
  operatorId <- findOperatorIdForDriver driverId
  let ratingSumKey = makeOperatorAnalyticsKey operatorId "ratingSum"
  -- Ensure Redis keys exist
  ensureRedisKeysExistForAllTime operatorId ratingSumKey

  -- Increment the rating sum
  void $ Redis.incrby ratingSumKey (fromIntegral ratingValue)

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
  operatorId <- findOperatorIdForDriver driverId
  let totalRideCountKey = makeOperatorAnalyticsKey operatorId "totalRideCount"
  -- Ensure Redis keys exist
  ensureRedisKeysExistForAllTime operatorId totalRideCountKey

  case newTotalRides of
    2 -> updateG1PeriodicKey operatorId
    11 -> updateG10PeriodicKey operatorId
    51 -> updateG50PeriodicKey operatorId
    _ -> pure ()

  -- Increment the total ride count
  void $ Redis.incr totalRideCountKey
  where
    updateG1PeriodicKey operatorId = do
      let kGt1Today = makeOperatorPeriodicKey operatorId "greaterThanOneRide" (show Today)
      let kGt1ThisWeek = makeOperatorPeriodicKey operatorId "greaterThanOneRide" (show ThisWeek)
      let kGt1ThisMonth = makeOperatorPeriodicKey operatorId "greaterThanOneRide" (show ThisMonth)
      ensureRedisKeysExistForPeriod operatorId (Redis.incr) kGt1Today Today
      ensureRedisKeysExistForPeriod operatorId (Redis.incr) kGt1ThisWeek ThisWeek
      ensureRedisKeysExistForPeriod operatorId (Redis.incr) kGt1ThisMonth ThisMonth
    updateG10PeriodicKey operatorId = do
      let kGt10Today = makeOperatorPeriodicKey operatorId "greaterThanTenRide" (show Today)
      let kGt10ThisWeek = makeOperatorPeriodicKey operatorId "greaterThanTenRide" (show ThisWeek)
      let kGt10ThisMonth = makeOperatorPeriodicKey operatorId "greaterThanTenRide" (show ThisMonth)
      ensureRedisKeysExistForPeriod operatorId (Redis.incr) kGt10Today Today
      ensureRedisKeysExistForPeriod operatorId (Redis.incr) kGt10ThisWeek ThisWeek
      ensureRedisKeysExistForPeriod operatorId (Redis.incr) kGt10ThisMonth ThisMonth
    updateG50PeriodicKey operatorId = do
      let kGt50Today = makeOperatorPeriodicKey operatorId "greaterThanFiftyRide" (show Today)
      let kGt50ThisWeek = makeOperatorPeriodicKey operatorId "greaterThanFiftyRide" (show ThisWeek)
      let kGt50ThisMonth = makeOperatorPeriodicKey operatorId "greaterThanFiftyRide" (show ThisMonth)
      ensureRedisKeysExistForPeriod operatorId (Redis.incr) kGt50Today Today
      ensureRedisKeysExistForPeriod operatorId (Redis.incr) kGt50ThisWeek ThisWeek
      ensureRedisKeysExistForPeriod operatorId (Redis.incr) kGt50ThisMonth ThisMonth

updateOperatorAnalyticsDriverEnabled ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Id DP.Person ->
  (Text -> m Integer) ->
  m ()
updateOperatorAnalyticsDriverEnabled driverId redisOps = do
  operatorId <- findOperatorIdForDriver driverId
  let driverEnabledTodayKey = makeOperatorPeriodicKey operatorId "driverEnabled" (show Today)
  let driverEnabledThisWeekKey = makeOperatorPeriodicKey operatorId "driverEnabled" (show ThisWeek)
  let driverEnabledThisMonthKey = makeOperatorPeriodicKey operatorId "driverEnabled" (show ThisMonth)
  ensureRedisKeysExistForPeriod operatorId redisOps driverEnabledTodayKey Today
  ensureRedisKeysExistForPeriod operatorId redisOps driverEnabledThisWeekKey ThisWeek
  ensureRedisKeysExistForPeriod operatorId redisOps driverEnabledThisMonthKey ThisMonth

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
incrementOperatorAnalyticsDriverEnabled driverId = updateOperatorAnalyticsDriverEnabled driverId (Redis.incr)

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
decrementOperatorAnalyticsDriverEnabled driverId = updateOperatorAnalyticsDriverEnabled driverId (Redis.decr)

updateOperatorAnalyticsApplicationCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  (Text -> m Integer) ->
  m ()
updateOperatorAnalyticsApplicationCount operatorId redisOps = do
  let totalApplicationCountTodayKey = makeOperatorPeriodicKey operatorId "totalApplicationCount" (show Today)
  let totalApplicationCountThisWeekKey = makeOperatorPeriodicKey operatorId "totalApplicationCount" (show ThisWeek)
  let totalApplicationCountThisMonthKey = makeOperatorPeriodicKey operatorId "totalApplicationCount" (show ThisMonth)
  ensureRedisKeysExistForPeriod operatorId redisOps totalApplicationCountTodayKey Today
  ensureRedisKeysExistForPeriod operatorId redisOps totalApplicationCountThisWeekKey ThisWeek
  ensureRedisKeysExistForPeriod operatorId redisOps totalApplicationCountThisMonthKey ThisMonth

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
incrementOperatorAnalyticsApplicationCount operatorId = updateOperatorAnalyticsApplicationCount operatorId (Redis.incr)

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
decrementOperatorAnalyticsApplicationCount operatorId = updateOperatorAnalyticsApplicationCount operatorId (Redis.decr)

-- | Redis key functions for fleet analytics
makeOperatorKeyPrefix :: Text -> Text
makeOperatorKeyPrefix operatorId = "operator:" <> operatorId <> ":"

makeOperatorAnalyticsKey :: Text -> Text -> Text
makeOperatorAnalyticsKey operatorId metric = makeOperatorKeyPrefix operatorId <> metric

makeOperatorPeriodicKey :: Text -> Text -> Text -> Text
makeOperatorPeriodicKey operatorId metric period = makeOperatorKeyPrefix operatorId <> metric <> ":" <> period

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
  Text ->
  Text ->
  Text ->
  m (Int, Int, Int)
fallbackToClickHouseAndUpdateRedisForAllTime operatorId totalRideKey ratingSumKey cancelCountKey = do
  logTagInfo "Fallback to ClickHouse" "true"
  driverIds <- SDFStatus.getFleetDriverIdsAndDriverIdsByOperatorId operatorId
  driverStats <- CDS.sumRatingAndTotalRidesByDriverIds driverIds
  let tr = driverStats.totalRidesSum
      rs = fromMaybe 0 driverStats.totalRatingScoreSum
      cc = driverStats.cancelledCount
  -- update redis (best-effort)
  void $ Redis.set totalRideKey tr
  void $ Redis.set ratingSumKey rs
  void $ Redis.set cancelCountKey cc
  pure (tr, rs, cc)

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
  m (Int, Int, Int, Int, Int)
fallbackComputePeriodOperatorAnalytics operatorId fromDay toDay = do
  driverIds <- SDFStatus.getFleetDriverIdsAndDriverIdsByOperatorId operatorId
  appCount <- SDFStatus.getTotalFleetDriverAndDriverCountByOperatorIdInDateRange operatorId (UTCTime fromDay 0) (UTCTime toDay 86399)
  enabledCount <- CDI.getEnabledDriverCountByDriverIds driverIds (UTCTime fromDay 0) (UTCTime toDay 86399)
  gt1 <- CDaily.countDriversWithNumRidesGreaterThan1Between driverIds fromDay toDay
  gt10 <- CDaily.countDriversWithNumRidesGreaterThan10Between driverIds fromDay toDay
  gt50 <- CDaily.countDriversWithNumRidesGreaterThan50Between driverIds fromDay toDay
  pure (appCount, enabledCount, gt1, gt10, gt50)

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
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Period ->
  Day ->
  Day ->
  m (Int, Int, Int, Int, Int)
fallbackToClickHouseAndUpdateRedisForPeriod operatorId kTotalApplicationCount kDriverEnabled kGtOne kGtTen kGtFifty period fromDay toDay = do
  logTagInfo "Fallback to ClickHouse (period)" "true"
  (appCount, enabledCount, gt1, gt10, gt50) <- fallbackComputePeriodOperatorAnalytics operatorId fromDay toDay
  now <- getCurrentTime
  -- Determine expiration time based on period
  let expireTime = case period of
        Today -> calculateSecondsUntilEndOfDay now
        Yesterday -> calculateSecondsUntilEndOfDay now
        ThisWeek -> calculateSecondsUntilEndOfWeek now
        LastWeek -> calculateSecondsUntilEndOfWeek now
        ThisMonth -> calculateSecondsUntilEndOfMonth now
        LastMonth -> calculateSecondsUntilEndOfMonth now

  -- write back to Redis using setNx with expiration
  void $ Redis.setExp kTotalApplicationCount appCount expireTime
  void $ Redis.setExp kDriverEnabled enabledCount expireTime
  void $ Redis.setExp kGtOne gt1 expireTime
  void $ Redis.setExp kGtTen gt10 expireTime
  void $ Redis.setExp kGtFifty gt50 expireTime
  pure (appCount, enabledCount, gt1, gt10, gt50)

inferPeriod :: UTCTime -> UTCTime -> UTCTime -> Maybe Period
inferPeriod now from to =
  let (y, m, d) = toGregorian (utctDay now)
      todayStart = UTCTime (fromGregorian y m d) 0
      -- week starts on Monday
      dow = calculateDayNumberOfWeek now
      thisWeekStart = addUTCTime (negate (fromIntegral dow * 86400)) todayStart
      firstOfMonth = UTCTime (fromGregorian y m 1) 0
      yesterdayStart = addUTCTime (-86400) todayStart
      lastWeekStart = addUTCTime (-7 * 86400) thisWeekStart
      lastMonthStart = UTCTime (addGregorianMonthsClip (-1) (fromGregorian y m 1)) 0
   in if from == todayStart && to <= now && to >= todayStart
        then Just Today
        else
          if from == thisWeekStart && to <= now && to >= todayStart
            then Just ThisWeek
            else
              if from == firstOfMonth && to <= now && to >= todayStart
                then Just ThisMonth
                else
                  if from == yesterdayStart && to == addUTCTime (-86400) todayStart
                    then Just Yesterday
                    else
                      if from == lastWeekStart && to == addUTCTime (-86400) thisWeekStart
                        then Just LastWeek
                        else
                          if from == lastMonthStart && to == addUTCTime (-86400) firstOfMonth
                            then Just LastMonth
                            else Nothing

data Period = Today | ThisWeek | ThisMonth | Yesterday | LastWeek | LastMonth
  deriving (Show, Eq)

calculateDayNumberOfWeek :: UTCTime -> Integer
calculateDayNumberOfWeek now = case dayOfWeek (utctDay now) of
  Monday -> 0
  Tuesday -> 1
  Wednesday -> 2
  Thursday -> 3
  Friday -> 4
  Saturday -> 5
  Sunday -> 6

calculateSecondsUntilEndOfDay :: UTCTime -> Int
calculateSecondsUntilEndOfDay now =
  let (y, m, d) = toGregorian (utctDay now)
      endOfDay = UTCTime (fromGregorian y m d) 86400 -- 23:59:59
   in round $ diffUTCTime endOfDay now

calculateSecondsUntilEndOfWeek :: UTCTime -> Int
calculateSecondsUntilEndOfWeek now =
  let (y, m, d) = toGregorian (utctDay now)
      dow = calculateDayNumberOfWeek now
      endOfWeek = addUTCTime (fromIntegral ((6 - dow) * 86400 + 86399)) (UTCTime (fromGregorian y m d) 0)
   in round $ diffUTCTime endOfWeek now

calculateSecondsUntilEndOfMonth :: UTCTime -> Int
calculateSecondsUntilEndOfMonth now =
  let (y, m, _) = toGregorian (utctDay now)
      firstOfNextMonth = addGregorianMonthsClip 1 (fromGregorian y m 1)
      endOfMonth = addUTCTime (-1) (UTCTime firstOfNextMonth 0)
   in round $ diffUTCTime endOfMonth now

deriveFromAndToDay :: Period -> UTCTime -> (Day, Day)
deriveFromAndToDay period now =
  let (y, m, _) = toGregorian (utctDay now)
      today = utctDay now
      yesterday = addDays (-1) today
      firstOfMonth = fromGregorian y m 1
      dow = calculateDayNumberOfWeek now
      thisWeekStart = addDays (negate dow) today
      lastWeekStart = addDays (-7) thisWeekStart
      lastWeekEnd = addDays (-1) thisWeekStart
      lastMonthStart = addGregorianMonthsClip (-1) firstOfMonth
      lastMonthEnd = addDays (-1) firstOfMonth
   in case period of
        Today -> (today, today)
        Yesterday -> (yesterday, yesterday)
        ThisWeek -> (thisWeekStart, today)
        LastWeek -> (lastWeekStart, lastWeekEnd)
        ThisMonth -> (firstOfMonth, today)
        LastMonth -> (lastMonthStart, lastMonthEnd)

handleCacheMissForOperatorAnalyticsAllTime ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  Text ->
  Text ->
  Text ->
  m (Int, Int, Int)
handleCacheMissForOperatorAnalyticsAllTime operatorId totalRideKey ratingSumKey cancelCountKey = do
  let inProgressKey = makeOperatorAnalyticsKey operatorId "inProgressAllTime"
  inProgress <- Redis.get @Bool inProgressKey
  case inProgress of
    Just True -> do
      logTagInfo "OperatorAnalyticsAllTime" $ "inProgress key present for operatorId: " <> operatorId <> ". Waiting for it to clear."
      SDFStatus.waitUntilKeyGone inProgressKey
      totalRide <- fromMaybe 0 <$> Redis.get @Int totalRideKey
      ratingSum <- fromMaybe 0 <$> Redis.get @Int ratingSumKey
      cancelCount <- fromMaybe 0 <$> Redis.get @Int cancelCountKey
      pure (totalRide, ratingSum, cancelCount)
    _ -> do
      lockAcquired <- Redis.setNxExpire inProgressKey 30 True -- 30 seconds expiry
      if lockAcquired
        then do
          logTagInfo "OperatorAnalyticsAllTime" $ "Acquired inProgress lock for operatorId: " <> operatorId <> ". Running ClickHouse query."
          res <- try @_ @SomeException $ fallbackToClickHouseAndUpdateRedisForAllTime operatorId totalRideKey ratingSumKey cancelCountKey
          Redis.del inProgressKey
          case res of
            Left err -> do
              logTagError "OperatorAnalyticsAllTime" $ "Error during ClickHouse/Redis operation for operatorId: " <> operatorId <> ". Error: " <> show err
              throwError (InternalError $ "Failed to perform operation for operatorId: " <> operatorId)
            Right allTimeRes -> pure allTimeRes
        else do
          logTagInfo "OperatorAnalyticsAllTime" $ "inProgress lock already held for operatorId: " <> operatorId <> " (race detected in else). Waiting for it to clear."
          handleCacheMissForOperatorAnalyticsAllTime operatorId totalRideKey ratingSumKey cancelCountKey

handleCacheMissForOperatorAnalyticsPeriod ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Period ->
  Day ->
  Day ->
  m (Int, Int, Int, Int, Int)
handleCacheMissForOperatorAnalyticsPeriod operatorId kAppCount kDriverEnabled kGt1 kGt10 kGt50 period fromDay toDay = do
  let inProgressKey = makeOperatorPeriodicKey operatorId "inProgressPeriod" (show period)
  inProgress <- Redis.get @Bool inProgressKey
  case inProgress of
    Just True -> do
      logTagInfo "OperatorAnalyticsPeriod" $ "inProgress key present for operatorId: " <> operatorId <> ". Waiting for it to clear."
      SDFStatus.waitUntilKeyGone inProgressKey
      appCount <- fromMaybe 0 <$> Redis.get @Int kAppCount
      enabledCount <- fromMaybe 0 <$> Redis.get @Int kDriverEnabled
      gt1 <- fromMaybe 0 <$> Redis.get @Int kGt1
      gt10 <- fromMaybe 0 <$> Redis.get @Int kGt10
      gt50 <- fromMaybe 0 <$> Redis.get @Int kGt50
      pure (appCount, enabledCount, gt1, gt10, gt50)
    _ -> do
      lockAcquired <- Redis.setNxExpire inProgressKey 30 True -- 30 seconds expiry
      if lockAcquired
        then do
          logTagInfo "OperatorAnalyticsPeriod" $ "Acquired inProgress lock for operatorId: " <> operatorId <> ". Running ClickHouse query."
          res <- try @_ @SomeException $ fallbackToClickHouseAndUpdateRedisForPeriod operatorId kAppCount kDriverEnabled kGt1 kGt10 kGt50 period fromDay toDay
          Redis.del inProgressKey
          case res of
            Left err -> do
              logTagError "OperatorAnalyticsPeriod" $ "Error during ClickHouse/Redis operation for operatorId: " <> operatorId <> ". Error: " <> show err
              throwError (InternalError $ "Failed to perform operation for operatorId: " <> operatorId)
            Right periodRes -> pure periodRes
        else do
          logTagInfo "OperatorAnalyticsPeriod" $ "inProgress lock already held for operatorId: " <> operatorId <> " (race detected in else). Waiting for it to clear."
          handleCacheMissForOperatorAnalyticsPeriod operatorId kAppCount kDriverEnabled kGt1 kGt10 kGt50 period fromDay toDay
