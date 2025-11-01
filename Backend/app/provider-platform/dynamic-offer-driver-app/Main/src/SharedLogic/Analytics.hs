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
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Types.DriverFlowStatus as DDF
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.TransporterConfig as TC
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverFlowStatus as SDFStatus
import qualified SharedLogic.FleetOperatorStats as SFleetOperatorStats
import qualified Storage.Clickhouse.DriverInformation as CDI
import qualified Storage.Clickhouse.FleetDriverAssociation as CFDA
import qualified Storage.Clickhouse.FleetOperatorStats as CFO
import qualified Storage.Clickhouse.Vehicle as CVehicle
import qualified Storage.Queries.DriverInformation as QDI
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

convertToAllTimeFallbackRes :: [(AllTimeMetric, Int)] -> CommonAllTimeFallbackRes
convertToAllTimeFallbackRes metricsList =
  let metricsMap = Map.fromList metricsList
      getMetricValue metric = Map.findWithDefault 0 metric metricsMap
   in OperatorAllTimeFallback $
        AllTimeFallbackRes
          { totalRideCount = getMetricValue TOTAL_RIDE_COUNT,
            ratingSum = getMetricValue RATING_SUM,
            cancelCount = getMetricValue CANCEL_COUNT,
            acceptationCount = getMetricValue ACCEPTATION_COUNT,
            totalRequestCount = getMetricValue TOTAL_REQUEST_COUNT
          }

data FleetAllTimeMetric = ACTIVE_DRIVER_COUNT | ACTIVE_VEHICLE_COUNT | CURRENT_ONLINE_DRIVER_COUNT
  deriving (Show, Eq, Ord)

fleetAllTimeMetrics :: [FleetAllTimeMetric]
fleetAllTimeMetrics = [ACTIVE_DRIVER_COUNT, ACTIVE_VEHICLE_COUNT, CURRENT_ONLINE_DRIVER_COUNT]

fleetAllTimeKeys :: Text -> [Text]
fleetAllTimeKeys fleetOperatorId = map (makeFleetAnalyticsKey fleetOperatorId) fleetAllTimeMetrics

data FleetAllTimeFallbackRes = FleetAllTimeFallbackRes
  { activeDriverCount :: Int,
    activeVehicleCount :: Int,
    currentOnlineDriverCount :: Int
  }
  deriving (Show, Eq)

convertToFleetAllTimeFallbackRes :: [(FleetAllTimeMetric, Int)] -> CommonAllTimeFallbackRes
convertToFleetAllTimeFallbackRes metricsList =
  let metricsMap = Map.fromList metricsList
      getMetricValue metric = Map.findWithDefault 0 metric metricsMap
   in FleetAllTimeFallback $
        FleetAllTimeFallbackRes
          { activeDriverCount = getMetricValue ACTIVE_DRIVER_COUNT,
            activeVehicleCount = getMetricValue ACTIVE_VEHICLE_COUNT,
            currentOnlineDriverCount = getMetricValue CURRENT_ONLINE_DRIVER_COUNT
          }

makeFleetKeyPrefix :: Text -> Text
makeFleetKeyPrefix fleetOperatorId = "fleetOwner:" <> fleetOperatorId <> ":"

makeFleetAnalyticsKey :: Text -> FleetAllTimeMetric -> Text
makeFleetAnalyticsKey fleetOperatorId metric = makeFleetKeyPrefix fleetOperatorId <> show metric

-- | Common datatype for all analytics fallback results
data CommonAllTimeFallbackRes
  = OperatorAllTimeFallback AllTimeFallbackRes
  | FleetAllTimeFallback FleetAllTimeFallbackRes
  deriving (Show, Eq)

-- | Helper function to extract fleet analytics data from common result
extractFleetAnalyticsData :: CommonAllTimeFallbackRes -> Flow (Int, Int, Int)
extractFleetAnalyticsData (FleetAllTimeFallback fleetData) = pure (fleetData.activeDriverCount, fleetData.activeVehicleCount, fleetData.currentOnlineDriverCount)
extractFleetAnalyticsData _ = throwError $ InvalidRequest "Expected FleetAllTimeFallback but got OperatorAllTimeFallback"

extractOperatorAnalyticsData :: CommonAllTimeFallbackRes -> Flow (Int, Int, Int, Int, Int)
extractOperatorAnalyticsData (OperatorAllTimeFallback operatorData) = pure (operatorData.totalRideCount, operatorData.ratingSum, operatorData.cancelCount, operatorData.acceptationCount, operatorData.totalRequestCount)
extractOperatorAnalyticsData _ = throwError $ InvalidRequest "Expected OperatorAllTimeFallback but got FleetAllTimeFallback"

data PeriodMetric = ACTIVE_DRIVER | DRIVER_ENABLED | GREATER_THAN_ONE_RIDE | GREATER_THAN_TEN_RIDE | GREATER_THAN_FIFTY_RIDE
  deriving (Show, Eq, Ord)

periodMetrics :: [PeriodMetric]
periodMetrics = [ACTIVE_DRIVER, DRIVER_ENABLED, GREATER_THAN_ONE_RIDE, GREATER_THAN_TEN_RIDE, GREATER_THAN_FIFTY_RIDE]

periodKeys :: Text -> Period -> [Text]
periodKeys operatorId period = map (\metric -> makeOperatorPeriodicKey operatorId metric period) periodMetrics

data PeriodFallbackRes = PeriodFallbackRes
  { activeDriver :: Int,
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
        { activeDriver = getMetricValue ACTIVE_DRIVER,
          driverEnabled = getMetricValue DRIVER_ENABLED,
          greaterThanOneRide = getMetricValue GREATER_THAN_ONE_RIDE,
          greaterThanTenRide = getMetricValue GREATER_THAN_TEN_RIDE,
          greaterThanFiftyRide = getMetricValue GREATER_THAN_FIFTY_RIDE
        }

-- | Redis key functions for operator analytics
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
-- Uses Person role to determine which fallback function to call
ensureRedisKeysExistForAllTimeCommon ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  DP.Role ->
  Text ->
  Text ->
  (Text -> Integer -> m Integer) ->
  Integer ->
  m ()
ensureRedisKeysExistForAllTimeCommon role entityId targetKey redisOps value = do
  mbValue <- Redis.get @Int targetKey
  (msg, logTag, allTimeKeysData, handleCacheMiss) <- case role of
    DP.OPERATOR ->
      pure
        ( "Key does not exist for operator alltime analytics key: " <> show targetKey <> ", operatorId: " <> show entityId,
          "AllTimeAnalytics",
          allTimeKeys entityId,
          handleCacheMissForAnalyticsAllTimeCommon DP.OPERATOR entityId
        )
    DP.FLEET_OWNER ->
      pure
        ( "Key does not exist for fleet alltime analytics key: " <> show targetKey <> ", fleetOwnerId: " <> show entityId,
          "AllTimeAnalyticsFleet",
          fleetAllTimeKeys entityId,
          handleCacheMissForAnalyticsAllTimeCommon DP.FLEET_OWNER entityId
        )
    _ -> throwError $ InvalidRequest $ "Unsupported role for analytics: " <> show role

  if isNothing mbValue
    then fork msg $ do
      logTagInfo logTag $ "Key does not exist. Handling cache miss for " <> show role <> "Id: " <> show entityId
      void $ handleCacheMiss allTimeKeysData
      void $ redisOps targetKey value
      logTagInfo logTag $ "Updated key after cache miss: key=" <> show targetKey <> ", " <> show role <> "Id=" <> show entityId
    else do
      logTagInfo logTag $ "Key already exists for " <> show role <> " analytics key: " <> show targetKey <> ", " <> show role <> "Id=" <> show entityId
      void $ redisOps targetKey value

ensureRedisKeysExistForPeriod ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  (Text -> m Integer) ->
  Text ->
  Period ->
  m ()
ensureRedisKeysExistForPeriod transporterConfig operatorId redisOps targetKey period = do
  mbValue <- Redis.get @Int targetKey
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime --Local time
  let msg = "Key does not exist for operator period analytics key: " <> show targetKey <> ", operatorId: " <> show operatorId

  if isNothing mbValue
    then fork msg $ do
      let (fromDay, toDay) = deriveFromAndToDay period now
          periodKeysData = periodKeys operatorId period
      logTagInfo "PeriodAnalytics" $ "Key does not exist. Handling cache miss for operatorId: " <> show operatorId
      void $ handleCacheMissForOperatorAnalyticsPeriod transporterConfig operatorId periodKeysData period fromDay toDay
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
  TC.TransporterConfig ->
  Id DP.Person ->
  m ()
updateOperatorAnalyticsCancelCount transporterConfig driverId = do
  -- Find the operator ID for this driver
  mbOperatorId <- findOperatorIdForDriver driverId
  when (isNothing mbOperatorId) $ logTagInfo "AnalyticsUpdateCancelCount" $ "No operator found for driver: " <> show driverId
  whenJust mbOperatorId $ \operatorId -> do
    let cancelCountKey = makeOperatorAnalyticsKey operatorId CANCEL_COUNT
    Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey operatorId SFleetOperatorStats.DRIVER_CANCEL) 10 5000 $
      SFleetOperatorStats.incrementDriverCancellationCount operatorId transporterConfig
    -- Ensure Redis keys exist
    ensureRedisKeysExistForAllTimeCommon DP.OPERATOR operatorId cancelCountKey Redis.incrby 1

  mbFleetOwner <- QFDA.findByDriverId driverId True
  when (isNothing mbFleetOwner) $ logTagInfo "AnalyticsUpdateCancelCount" $ "No fleet owner found for driver: " <> show driverId
  whenJust mbFleetOwner $ \fleetOwner -> do
    Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwner.fleetOwnerId SFleetOperatorStats.DRIVER_CANCEL) 10 5000 $ do
      SFleetOperatorStats.incrementDriverCancellationCount fleetOwner.fleetOwnerId transporterConfig
      SFleetOperatorStats.incrementDriverCancellationCountDaily fleetOwner.fleetOwnerId transporterConfig

updateFleetOwnerAnalyticsCustomerCancelCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Id DP.Person ->
  TC.TransporterConfig ->
  m ()
updateFleetOwnerAnalyticsCustomerCancelCount driverId transporterConfig = do
  mbFleetOwner <- QFDA.findByDriverId driverId True
  when (isNothing mbFleetOwner) $ logTagInfo "AnalyticsUpdateCustomerCancelCount" $ "No fleet owner found for driver: " <> show driverId
  whenJust mbFleetOwner $ \fleetOwner -> do
    Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwner.fleetOwnerId SFleetOperatorStats.CUSTOMER_CANCEL) 10 5000 $ do
      SFleetOperatorStats.incrementCustomerCancellationCount fleetOwner.fleetOwnerId transporterConfig
      SFleetOperatorStats.incrementCustomerCancellationCountDaily fleetOwner.fleetOwnerId transporterConfig

updateOperatorAnalyticsAcceptationTotalRequestAndPassedCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Id DP.Person ->
  TC.TransporterConfig ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  m ()
updateOperatorAnalyticsAcceptationTotalRequestAndPassedCount driverId transporterConfig incrementTotalRequestCount incrementAcceptationCount incrementRejectedRequestCount incrementPulledRequestCount = do
  mbOperatorId <- findOperatorIdForDriver driverId
  when (isNothing mbOperatorId) $ logTagInfo "AnalyticsUpdateAcceptationAndTotalRequestCount" $ "No operator found for driver: " <> show driverId
  whenJust mbOperatorId $ \operatorId -> do
    when incrementAcceptationCount $ updateAcceptationCount operatorId
    when incrementTotalRequestCount $ updateTotalRequestCount operatorId

  mbFleetOwner <- QFDA.findByDriverId driverId True
  when (isNothing mbFleetOwner) $ logTagInfo "AnalyticsUpdateAcceptationAndTotalRequestCount" $ "No fleet owner found for driver: " <> show driverId
  whenJust mbFleetOwner $ \fleetOwner -> do
    when incrementRejectedRequestCount $ do
      Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwner.fleetOwnerId SFleetOperatorStats.REJECTED_REQUEST) 10 5000 $
        SFleetOperatorStats.incrementRejectedRequestCountDaily fleetOwner.fleetOwnerId (transporterConfig :: TC.TransporterConfig) -- RejectedRequestCount only store in Fleet Daily Stats
    when incrementPulledRequestCount $ do
      Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwner.fleetOwnerId SFleetOperatorStats.PULLED_REQUEST) 10 5000 $
        SFleetOperatorStats.incrementPulledRequestCountDaily fleetOwner.fleetOwnerId (transporterConfig :: TC.TransporterConfig) -- PulledRequestCount only store in Fleet Daily Stats
    when incrementTotalRequestCount $ do
      Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwner.fleetOwnerId SFleetOperatorStats.TOTAL_REQUEST) 10 5000 $ do
        SFleetOperatorStats.incrementTotalRequestCount fleetOwner.fleetOwnerId (transporterConfig :: TC.TransporterConfig)
        SFleetOperatorStats.incrementTotalRequestCountDaily fleetOwner.fleetOwnerId (transporterConfig :: TC.TransporterConfig)
    when incrementAcceptationCount $ do
      Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwner.fleetOwnerId SFleetOperatorStats.ACCEPTATION_REQUEST) 10 5000 $ do
        SFleetOperatorStats.incrementAcceptationRequestCount fleetOwner.fleetOwnerId (transporterConfig :: TC.TransporterConfig)
        SFleetOperatorStats.incrementAcceptationRequestCountDaily fleetOwner.fleetOwnerId (transporterConfig :: TC.TransporterConfig)
  where
    updateAcceptationCount operatorId = do
      let acceptationCountKey = makeOperatorAnalyticsKey operatorId ACCEPTATION_COUNT
      Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey operatorId SFleetOperatorStats.ACCEPTATION_REQUEST) 10 5000 $
        SFleetOperatorStats.incrementAcceptationRequestCount operatorId (transporterConfig :: TC.TransporterConfig)
      ensureRedisKeysExistForAllTimeCommon DP.OPERATOR operatorId acceptationCountKey Redis.incrby 1
    updateTotalRequestCount operatorId = do
      let totalRequestCountKey = makeOperatorAnalyticsKey operatorId TOTAL_REQUEST_COUNT
      Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey operatorId SFleetOperatorStats.TOTAL_REQUEST) 10 5000 $
        SFleetOperatorStats.incrementTotalRequestCount operatorId (transporterConfig :: TC.TransporterConfig)
      ensureRedisKeysExistForAllTimeCommon DP.OPERATOR operatorId totalRequestCountKey Redis.incrby 1

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
  TC.TransporterConfig ->
  Int ->
  Bool ->
  m ()
updateOperatorAnalyticsRatingScoreKey driverId transporterConfig ratingValue shouldIncrementCount = do
  mbOperatorId <- findOperatorIdForDriver driverId
  when (isNothing mbOperatorId) $ logTagInfo "AnalyticsUpdateRatingScoreKey" $ "No operator found for driver: " <> show driverId

  -- Operator analytics
  whenJust mbOperatorId $ \operatorId -> do
    let ratingSumKey = makeOperatorAnalyticsKey operatorId RATING_SUM
    Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey operatorId SFleetOperatorStats.RATING_COUNT_AND_SCORE) 10 5000 $
      SFleetOperatorStats.incrementTotalRatingCountAndTotalRatingScore operatorId transporterConfig ratingValue shouldIncrementCount
    -- Ensure Redis keys exist
    ensureRedisKeysExistForAllTimeCommon DP.OPERATOR operatorId ratingSumKey Redis.incrby (fromIntegral ratingValue)

  -- Fleet owner analytics
  mbFLeetOwner <- QFDA.findByDriverId driverId True
  when (isNothing mbFLeetOwner) $ logTagInfo "AnalyticsUpdateRatingScoreKey" $ "No fleet owner found for driver: " <> show driverId
  whenJust mbFLeetOwner $ \fleetOwner -> do
    Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwner.fleetOwnerId SFleetOperatorStats.RATING_COUNT_AND_SCORE) 10 5000 $ do
      SFleetOperatorStats.incrementTotalRatingCountAndTotalRatingScore fleetOwner.fleetOwnerId transporterConfig ratingValue shouldIncrementCount
      SFleetOperatorStats.incrementTotalRatingCountAndTotalRatingScoreDaily fleetOwner.fleetOwnerId transporterConfig ratingValue shouldIncrementCount

updateOperatorAnalyticsTotalRideCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Id DP.Person ->
  DR.Ride ->
  m ()
updateOperatorAnalyticsTotalRideCount transporterConfig driverId ride = do
  mbOperatorId <- findOperatorIdForDriver driverId
  when (isNothing mbOperatorId) $ logTagInfo "AnalyticsUpdateTotalRideCount" $ "No operator found for driver: " <> show driverId
  whenJust mbOperatorId $ \operatorId -> do
    let totalRideCountKey = makeOperatorAnalyticsKey operatorId TOTAL_RIDE_COUNT
    Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey operatorId SFleetOperatorStats.RIDE_METRICS) 10 5000 $
      SFleetOperatorStats.incrementTotalRidesTotalDistAndTotalEarning operatorId ride transporterConfig
    -- Ensure Redis keys exist
    ensureRedisKeysExistForAllTimeCommon DP.OPERATOR operatorId totalRideCountKey Redis.incrby 1

  mbFLeetOwner <- QFDA.findByDriverId driverId True
  when (isNothing mbFLeetOwner) $ logTagInfo "AnalyticsUpdateTotalRideCount" $ "No fleet owner found for driver: " <> show driverId
  whenJust mbFLeetOwner $ \fleetOwner -> do
    Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwner.fleetOwnerId SFleetOperatorStats.RIDE_METRICS) 10 5000 $ do
      SFleetOperatorStats.incrementTotalRidesTotalDistAndTotalEarning fleetOwner.fleetOwnerId ride transporterConfig
      SFleetOperatorStats.incrementTotalEarningDistanceAndCompletedRidesDaily fleetOwner.fleetOwnerId ride transporterConfig

-- case newTotalRides of
--   2 -> updatePeriodicMetrics transporterConfig operatorId GREATER_THAN_ONE_RIDE Redis.incr
--   11 -> updatePeriodicMetrics transporterConfig operatorId GREATER_THAN_TEN_RIDE Redis.incr
--   51 -> updatePeriodicMetrics transporterConfig operatorId GREATER_THAN_FIFTY_RIDE Redis.incr
--   _ -> pure ()

updatePeriodicMetrics ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  PeriodMetric ->
  (Text -> m Integer) ->
  m ()
updatePeriodicMetrics transporterConfig operatorId metric redisOps = do
  let periods = [Today, ThisWeek, ThisMonth]
      updateForPeriod period = do
        let key = makeOperatorPeriodicKey operatorId metric period
        ensureRedisKeysExistForPeriod transporterConfig operatorId redisOps key period
  mapM_ updateForPeriod periods

incrementOperatorAnalyticsDriverEnabled ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  m ()
incrementOperatorAnalyticsDriverEnabled transporterConfig operatorId = do
  updatePeriodicMetrics transporterConfig operatorId DRIVER_ENABLED Redis.incr

decrementOperatorAnalyticsDriverEnabled ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  m ()
decrementOperatorAnalyticsDriverEnabled transporterConfig operatorId = do
  updatePeriodicMetrics transporterConfig operatorId DRIVER_ENABLED Redis.decr

updateEnabledVerifiedStateWithAnalytics :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, HasField "serviceClickhouseCfg" r CH.ClickhouseCfg, HasField "serviceClickhouseEnv" r CH.ClickhouseEnv) => Maybe DI.DriverInformation -> TC.TransporterConfig -> Id DP.Person -> Bool -> Maybe Bool -> m ()
updateEnabledVerifiedStateWithAnalytics mbDriverInfoData transporterConfig driverId isEnabled isVerified = do
  when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ do
    mbDriverInfo <- case mbDriverInfoData of
      Just driverInfoData -> pure (Just driverInfoData)
      Nothing -> QDI.findById driverId
    when (isNothing mbDriverInfo) $ logTagError "AnalyticsUpdateEnabledVerifiedState" $ "No driver info found for driver: " <> show driverId
    whenJust mbDriverInfo $ \driverInfo -> updateDriverEnabledState driverInfo
  QDI.updateEnabledVerifiedState driverId isEnabled isVerified
  where
    updateDriverEnabledState driverInfo = do
      when (driverInfo.enabled /= isEnabled) $ do
        mbOperatorId <- findOperatorIdForDriver driverId
        when (isNothing mbOperatorId) $ logTagError "AnalyticsUpdateDriverEnabled" $ "No operator found for driver: " <> show driverId
        whenJust mbOperatorId $ \operatorId ->
          if isEnabled
            then incrementOperatorAnalyticsDriverEnabled transporterConfig operatorId
            else decrementOperatorAnalyticsDriverEnabled transporterConfig operatorId

incrementOperatorAnalyticsActiveDriver ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  m ()
incrementOperatorAnalyticsActiveDriver transporterConfig operatorId =
  updatePeriodicMetrics transporterConfig operatorId ACTIVE_DRIVER Redis.incr

decrementOperatorAnalyticsActiveDriver ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  m ()
decrementOperatorAnalyticsActiveDriver transporterConfig operatorId =
  updatePeriodicMetrics transporterConfig operatorId ACTIVE_DRIVER Redis.decr

-- Updation Logic of Fleet Owner Fields
incrementFleetOwnerAnalyticsActiveDriverCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Maybe Text ->
  Id DP.Person ->
  m ()
incrementFleetOwnerAnalyticsActiveDriverCount mbFleetOwnerId driverId = do
  case mbFleetOwnerId of
    Just fleetOwnerId -> incrementActiveDriverCount fleetOwnerId
    Nothing -> do
      mbFleetOwner <- QFDA.findByDriverId driverId True
      when (isNothing mbFleetOwner) $ logTagError "AnalyticsUpdateActiveDriverCount" $ "No fleet owner found for driver: " <> show driverId
      whenJust mbFleetOwner $ \fleetOwner -> incrementActiveDriverCount fleetOwner.fleetOwnerId
  where
    incrementActiveDriverCount fleetOwnerId = do
      let totalActiveDriverCountKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_DRIVER_COUNT
      ensureRedisKeysExistForAllTimeCommon DP.FLEET_OWNER fleetOwnerId totalActiveDriverCountKey Redis.incrby 1

decrementFleetOwnerAnalyticsActiveDriverCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Maybe Text ->
  Id DP.Person ->
  m ()
decrementFleetOwnerAnalyticsActiveDriverCount mbFleetOwnerId driverId = do
  case mbFleetOwnerId of
    Just fleetOwnerId -> decrementActiveDriverCount fleetOwnerId
    Nothing -> do
      mbFleetOwner <- QFDA.findByDriverId driverId True
      when (isNothing mbFleetOwner) $ logTagError "AnalyticsUpdateActiveDriverCount" $ "No fleet owner found for driver: " <> show driverId
      whenJust mbFleetOwner $ \fleetOwner -> decrementActiveDriverCount fleetOwner.fleetOwnerId
  where
    decrementActiveDriverCount fleetOwnerId = do
      let totalActiveDriverCountKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_DRIVER_COUNT
      ensureRedisKeysExistForAllTimeCommon DP.FLEET_OWNER fleetOwnerId totalActiveDriverCountKey Redis.decrby 1

incrementFleetOwnerAnalyticsActiveVehicleCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Maybe Text ->
  Id DP.Person ->
  m ()
incrementFleetOwnerAnalyticsActiveVehicleCount mbFleetOwnerId driverId = do
  case mbFleetOwnerId of
    Just fleetOwnerId -> incrementActiveVehicleCount fleetOwnerId
    Nothing -> do
      mbFleetOwner <- QFDA.findByDriverId driverId True
      when (isNothing mbFleetOwner) $ logTagError "AnalyticsUpdateActiveVehicleCount" $ "No fleet owner found for driver: " <> show driverId
      whenJust mbFleetOwner $ \fleetOwner -> incrementActiveVehicleCount fleetOwner.fleetOwnerId
  where
    incrementActiveVehicleCount fleetOwnerId = do
      let totalActiveVehicleCountKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_VEHICLE_COUNT
      ensureRedisKeysExistForAllTimeCommon DP.FLEET_OWNER fleetOwnerId totalActiveVehicleCountKey Redis.incrby 1

decrementFleetOwnerAnalyticsActiveVehicleCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Maybe Text ->
  Id DP.Person ->
  m ()
decrementFleetOwnerAnalyticsActiveVehicleCount mbFleetOwnerId driverId = do
  case mbFleetOwnerId of
    Just fleetOwnerId -> decrementActiveVehicleCount fleetOwnerId
    Nothing -> do
      mbFleetOwner <- QFDA.findByDriverId driverId True
      when (isNothing mbFleetOwner) $ logTagError "AnalyticsUpdateActiveVehicleCount" $ "No fleet owner found for driver: " <> show driverId
      whenJust mbFleetOwner $ \fleetOwner -> decrementActiveVehicleCount fleetOwner.fleetOwnerId
  where
    decrementActiveVehicleCount fleetOwnerId = do
      let totalActiveVehicleCountKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_VEHICLE_COUNT
      ensureRedisKeysExistForAllTimeCommon DP.FLEET_OWNER fleetOwnerId totalActiveVehicleCountKey Redis.decrby 1

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
  m CommonAllTimeFallbackRes
fallbackToClickHouseAndUpdateRedisForAllTime operatorId allTimeKeysData = do
  logTagInfo "FallbackClickhouseAllTime" $ "Initiating ClickHouse query to retrieve analytics data for operator ID: " <> operatorId
  operatorStats <- CFO.sumStatsByFleetOperatorId operatorId
  let tcr = fromMaybe 0 operatorStats.totalCompletedRidesSum
      rs = fromMaybe 0 operatorStats.totalRatingScoreSum
      dcc = fromMaybe 0 operatorStats.driverCancellationCountSum
      ac = fromMaybe 0 operatorStats.acceptationRequestCountSum
      trc = fromMaybe 0 operatorStats.totalRequestCountSum
  -- update redis (best-effort)
  mapM_ (uncurry Redis.set) (zip allTimeKeysData [tcr, rs, dcc, ac, trc])
  pure $ convertToAllTimeFallbackRes (zip allTimeMetrics [tcr, rs, dcc, ac, trc])

fallbackToClickHouseAndUpdateRedisForAllTimeFleet ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  [Text] ->
  m CommonAllTimeFallbackRes
fallbackToClickHouseAndUpdateRedisForAllTimeFleet fleetOwnerId fleetAllTimeKeysData = do
  logTagInfo "FallbackClickhouseAllTimeFleet" $ "Initiating ClickHouse query to retrieve analytics data for fleetOwnerId: " <> fleetOwnerId
  driverIds <- CFDA.getDriverIdsByFleetOwnerId fleetOwnerId
  let activeDriverCount = length driverIds
  activeVehicleCount <- CVehicle.countByDriverIds driverIds
  currentOnlineDriverCount <- getOnlineDriverCount
  -- update redis (best-effort)
  mapM_ (uncurry Redis.set) (zip fleetAllTimeKeysData [activeDriverCount, activeVehicleCount, currentOnlineDriverCount])
  pure $ convertToFleetAllTimeFallbackRes (zip fleetAllTimeMetrics [activeDriverCount, activeVehicleCount, currentOnlineDriverCount])
  where
    getOnlineDriverCount = do
      let onlineKey = DDF.getStatusKey fleetOwnerId DDF.ONLINE
      res <- Redis.get @Int onlineKey
      if isNothing res
        then do
          void $ SDFStatus.handleCacheMissForDriverFlowStatus DP.FLEET_OWNER fleetOwnerId (DDF.allKeys fleetOwnerId)
          onlineRes <- Redis.get @Int onlineKey
          pure (fromMaybe 0 onlineRes)
        else pure (fromMaybe 0 res)

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
  activeDriver <- SDFStatus.getTotalFleetDriverAndDriverCountByOperatorIdInDateRange operatorId (UTCTime fromDay 0) (UTCTime toDay 86399)
  enabledCount <- CDI.getEnabledDriverCountByDriverIds driverIds (UTCTime fromDay 0) (UTCTime toDay 86399)
  -- gt1 <- CDaily.countDriversWithNumRidesGreaterThan1Between driverIds fromDay toDay
  -- gt10 <- CDaily.countDriversWithNumRidesGreaterThan10Between driverIds fromDay toDay
  -- gt50 <- CDaily.countDriversWithNumRidesGreaterThan50Between driverIds fromDay toDay
  pure PeriodFallbackRes {activeDriver, driverEnabled = enabledCount, greaterThanOneRide = 0 :: Int, greaterThanTenRide = 0 :: Int, greaterThanFiftyRide = 0 :: Int} -- TODO: Need to discuss how to calculate this greater than any ride metrics

-- | Fallback to ClickHouse for period analytics and update Redis keys for the period
fallbackToClickHouseAndUpdateRedisForPeriod ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  [Text] ->
  Period ->
  Day ->
  Day ->
  m PeriodFallbackRes
fallbackToClickHouseAndUpdateRedisForPeriod transporterConfig operatorId periodKeysData period fromDay toDay = do
  logTagInfo "FallbackClickhousePeriod" $ "Initiating ClickHouse query to retrieve analytics data for operator ID: " <> operatorId <> " for period: " <> show period
  res <- fallbackComputePeriodOperatorAnalytics operatorId fromDay toDay
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime --Local time
  let expireTime = getPeriodExpireTime period now

  -- write back to Redis using setNx with expiration
  mapM_ (\(key, value) -> Redis.setExp key value expireTime) (zip periodKeysData [res.activeDriver, res.driverEnabled, res.greaterThanOneRide, res.greaterThanTenRide, res.greaterThanFiftyRide])
  pure res

-- | Common function to handle cache miss for analytics using Person role
handleCacheMissForAnalyticsAllTimeCommon ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  DP.Role ->
  Text ->
  [Text] ->
  m CommonAllTimeFallbackRes
handleCacheMissForAnalyticsAllTimeCommon role entityId allTimeKeysData = do
  (inProgressKey, logTag, fallbackFunc) <- case role of
    DP.OPERATOR ->
      pure
        ( makeOperatorKeyPrefix entityId <> "inProgressAllTime",
          "OperatorAnalyticsAllTime",
          fallbackToClickHouseAndUpdateRedisForAllTime entityId
        )
    DP.FLEET_OWNER ->
      pure
        ( makeFleetKeyPrefix entityId <> "inProgressAllTime",
          "FleetAnalyticsAllTime",
          fallbackToClickHouseAndUpdateRedisForAllTimeFleet entityId
        )
    _ -> throwError $ InvalidRequest $ "Unsupported role for analytics: " <> show role

  inProgress <- Redis.get @Bool inProgressKey
  case inProgress of
    Just True -> do
      logTagInfo logTag $ "inProgress key present for " <> show role <> "Id: " <> entityId <> ". Waiting for it to clear."
      SDFStatus.waitUntilKeyGone inProgressKey
      allTimeKeysRes <- mapM (\key -> fromMaybe 0 <$> Redis.get @Int key) allTimeKeysData
      pure $ case role of
        DP.OPERATOR -> convertToAllTimeFallbackRes (zip allTimeMetrics allTimeKeysRes)
        DP.FLEET_OWNER -> convertToFleetAllTimeFallbackRes (zip fleetAllTimeMetrics allTimeKeysRes)
        _ -> error $ "Unsupported role for analytics: " <> show role
    _ -> do
      lockAcquired <- Redis.setNxExpire inProgressKey 60 True -- 60 seconds expiry
      if lockAcquired
        then do
          logTagInfo logTag $ "Acquired inProgress lock for " <> show role <> "Id: " <> entityId <> ". Running ClickHouse query."
          res <- try @_ @SomeException $ fallbackFunc allTimeKeysData
          Redis.del inProgressKey
          case res of
            Left err -> do
              logTagError logTag $ "Error during ClickHouse/Redis operation for " <> show role <> "Id: " <> entityId <> ". Error: " <> show err
              throwError (InternalError $ "Failed to perform operation for " <> show role <> "Id: " <> entityId)
            Right allTimeRes -> pure allTimeRes
        else do
          logTagInfo logTag $ "inProgress lock already held for " <> show role <> "Id: " <> entityId <> " (race detected in else). Waiting for it to clear."
          handleCacheMissForAnalyticsAllTimeCommon role entityId allTimeKeysData

handleCacheMissForOperatorAnalyticsPeriod ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  [Text] ->
  Period ->
  Day ->
  Day ->
  m PeriodFallbackRes
handleCacheMissForOperatorAnalyticsPeriod transporterConfig operatorId periodKeysData period fromDay toDay = do
  let inProgressKey = makeOperatorKeyPrefix operatorId <> "inProgressPeriod" <> show period
  inProgress <- Redis.get @Bool inProgressKey
  case inProgress of
    Just True -> do
      logTagInfo "OperatorAnalyticsPeriod" $ "inProgress key present for operatorId: " <> operatorId <> ". Waiting for it to clear."
      SDFStatus.waitUntilKeyGone inProgressKey
      periodKeysRes <- mapM (\key -> fromMaybe 0 <$> Redis.get @Int key) periodKeysData
      pure $ convertToPeriodFallbackRes (zip periodMetrics periodKeysRes)
    _ -> do
      lockAcquired <- Redis.setNxExpire inProgressKey 60 True -- 60 seconds expiry
      if lockAcquired
        then do
          logTagInfo "OperatorAnalyticsPeriod" $ "Acquired inProgress lock for operatorId: " <> operatorId <> ". Running ClickHouse query."
          res <- try @_ @SomeException $ fallbackToClickHouseAndUpdateRedisForPeriod transporterConfig operatorId periodKeysData period fromDay toDay
          Redis.del inProgressKey
          case res of
            Left err -> do
              logTagError "OperatorAnalyticsPeriod" $ "Error during ClickHouse/Redis operation for operatorId: " <> operatorId <> ". Error: " <> show err
              throwError (InternalError $ "Failed to perform operation for operatorId: " <> operatorId)
            Right periodRes -> pure periodRes
        else do
          logTagInfo "OperatorAnalyticsPeriod" $ "inProgress lock already held for operatorId: " <> operatorId <> " (race detected in else). Waiting for it to clear."
          handleCacheMissForOperatorAnalyticsPeriod transporterConfig operatorId periodKeysData period fromDay toDay

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

      endOfDay = UTCTime (fromGregorian y m d) 86399 -- 23:59:59
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

-- | Common function to handle driver analytics and flow status caching
handleDriverAnalyticsAndFlowStatus ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Id DP.Person ->
  Maybe DI.DriverInformation ->
  (DI.DriverInformation -> m ()) -> -- analytics action to perform
  (DI.DriverInformation -> m ()) -> -- flow status action to perform
  m ()
handleDriverAnalyticsAndFlowStatus transporterConfig driverId mbDriverInfo analyticsAction flowStatusAction = do
  let allowCacheDriverFlowStatus = transporterConfig.analyticsConfig.allowCacheDriverFlowStatus
  let needsDriverInfo = transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics || allowCacheDriverFlowStatus

  when needsDriverInfo $ do
    driverInfo <- case mbDriverInfo of
      Just driverInfo -> pure driverInfo
      Nothing -> QDI.findById driverId >>= fromMaybeM (DriverNotFound driverId.getId)

    when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ do
      analyticsAction driverInfo

    when allowCacheDriverFlowStatus $ do
      flowStatusAction driverInfo
