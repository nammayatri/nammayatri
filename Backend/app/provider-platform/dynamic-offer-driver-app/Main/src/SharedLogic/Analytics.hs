{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Analytics where

import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.DriverFlowStatus as DDF
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.Ride as DRide
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
import qualified Domain.Types.SubscriptionPurchase as DSP
import qualified Storage.Clickhouse.FleetOperatorDailyStats as CFleetOpDailyStats
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSubscriptionPurchaseExtra
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.FleetOperatorDailyStatsExtra as QFleetOpsDailyExtra
import SharedLogic.AnalyticsExtra
import Tools.Error

-- | Update analytics and driver stats counters for a cancelled ride.
updateCancellationAnalyticsAndDriverStats ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  DRide.Ride ->
  SBCR.BookingCancellationReason ->
  m ()
updateCancellationAnalyticsAndDriverStats transporterConfig ride bookingCReason = do
  driverStats <- QDriverStats.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  case bookingCReason.source of
    SBCR.ByDriver -> do
      when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $
        updateOperatorAnalyticsCancelCount transporterConfig ride.driverId
      QDriverStats.updateValidDriverCancellationTagCount (driverStats.validDriverCancellationTagCount + 1) ride.driverId
    SBCR.ByUser -> do
      when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $
        updateFleetOwnerAnalyticsCustomerCancelCount ride.driverId transporterConfig
      QDriverStats.updateValidCustomerCancellationTagCount (driverStats.validCustomerCancellationTagCount + 1) ride.driverId
    _ -> pure ()

-- | Update fleet owner analytics counters in Redis. Passing Nothing deletes the key.
updateFleetOwnerAnalyticsKeys ::
  (Redis.HedisFlow m r, MonadFlow m) =>
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  m ()
updateFleetOwnerAnalyticsKeys fleetOwnerId mbActiveDrivers mbActiveVehicles mbCurrentOnline = do
  let setOrDel key = \case
        Just v -> Redis.set key v
        Nothing -> Redis.del key >> pure ()

  -- active driver count
  let adcKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_DRIVER_COUNT
  setOrDel adcKey mbActiveDrivers

  -- active vehicle count
  let avcKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_VEHICLE_COUNT
  setOrDel avcKey mbActiveVehicles

  -- current online driver count uses ONLINE status key
  let codKey = DDF.getStatusKey fleetOwnerId DDF.ONLINE
  setOrDel codKey mbCurrentOnline

-- | Helper function to extract fleet analytics data from common result
extractFleetAnalyticsData :: CommonAllTimeFallbackRes -> Flow (Maybe Int, Maybe Int, Maybe Int)
extractFleetAnalyticsData (FleetAllTimeFallback fleetData) = pure (fleetData.activeDriverCount, fleetData.activeVehicleCount, fleetData.currentOnlineDriverCount)
extractFleetAnalyticsData _ = throwError $ InvalidRequest "Expected FleetAllTimeFallback but got OperatorAllTimeFallback"

extractOperatorAnalyticsData :: CommonAllTimeFallbackRes -> Flow (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int)
extractOperatorAnalyticsData (OperatorAllTimeFallback operatorData) =
  pure
    ( operatorData.totalRideCount,
      operatorData.ratingSum,
      operatorData.ratingCount,
      operatorData.cancelCount,
      operatorData.acceptationCount,
      operatorData.totalRequestCount,
      operatorData.totalAssociatedDriver,
      operatorData.totalActiveDrivers,
      operatorData.totalEnabledDrivers
    )
extractOperatorAnalyticsData _ = throwError $ InvalidRequest "Expected OperatorAllTimeFallback but got FleetAllTimeFallback"

data PeriodRes = PeriodRes
  { activeDriver :: Int,
    driverEnabled :: Int,
    greaterThanOneRide :: Int,
    greaterThanTenRide :: Int,
    greaterThanFiftyRide :: Int,
    approvedDriverInspection :: Int,
    approvedVehicleInspection :: Int,
    rejectedDriverInspection :: Int,
    rejectedVehicleInspection :: Int
  }
  deriving (Show, Eq)
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
  -- Find all operator IDs for this driver
  operatorIds <- findOperatorIdForDriver driverId
  when (null operatorIds) $ logTagInfo "AnalyticsUpdateCancelCount" $ "No operator found for driver: " <> show driverId
  forM_ operatorIds $ \operatorId -> do
    let cancelCountKey = makeOperatorAnalyticsKey operatorId CANCEL_COUNT
    Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey operatorId) 10 5000 $
      SFleetOperatorStats.incrementDriverCancellationCount operatorId transporterConfig
    -- Ensure Redis keys exist
    ensureRedisKeysExistForAllTimeCommon transporterConfig DP.OPERATOR operatorId cancelCountKey Redis.incrby 1

  mbFleetOwner <- QFDA.findByDriverId driverId True
  when (isNothing mbFleetOwner) $ logTagInfo "AnalyticsUpdateCancelCount" $ "No fleet owner found for driver: " <> show driverId
  whenJust mbFleetOwner $ \fleetOwner -> do
    Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwner.fleetOwnerId) 10 5000 $ do
      SFleetOperatorStats.incrementDriverCancellationCount fleetOwner.fleetOwnerId transporterConfig
      SFleetOperatorStats.incrementDriverCancellationCountDaily fleetOwner.fleetOwnerId driverId.getId transporterConfig

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
    Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwner.fleetOwnerId) 10 5000 $ do
      SFleetOperatorStats.incrementCustomerCancellationCount fleetOwner.fleetOwnerId transporterConfig
      SFleetOperatorStats.incrementCustomerCancellationCountDaily fleetOwner.fleetOwnerId driverId.getId transporterConfig

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
  operatorIds <- findOperatorIdForDriver driverId
  when (null operatorIds) $ logTagInfo "AnalyticsUpdateAcceptationAndTotalRequestCount" $ "No operator found for driver: " <> show driverId
  forM_ operatorIds $ \operatorId -> do
    when (incrementTotalRequestCount || incrementAcceptationCount) $ do
      Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey operatorId) 10 5000 $
        SFleetOperatorStats.incrementRequestCounts
          operatorId
          (transporterConfig :: TC.TransporterConfig)
          incrementAcceptationCount
          incrementTotalRequestCount
    when incrementAcceptationCount $ updateAcceptationCountRedisKey operatorId
    when incrementTotalRequestCount $ updateTotalRequestCountRedisKey operatorId

  mbFleetOwner <- QFDA.findByDriverId driverId True
  when (isNothing mbFleetOwner) $ logTagInfo "AnalyticsUpdateAcceptationAndTotalRequestCount" $ "No fleet owner found for driver: " <> show driverId
  whenJust mbFleetOwner $ \fleetOwner -> do
    when (incrementRejectedRequestCount || incrementPulledRequestCount || incrementTotalRequestCount || incrementAcceptationCount) $
      Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwner.fleetOwnerId) 10 5000 $ do
        -- Update overall stats
        when (incrementTotalRequestCount || incrementAcceptationCount) $
          SFleetOperatorStats.incrementRequestCounts
            fleetOwner.fleetOwnerId
            (transporterConfig :: TC.TransporterConfig)
            incrementAcceptationCount
            incrementTotalRequestCount
        -- Update daily stats
        SFleetOperatorStats.incrementRequestCountsDaily
          fleetOwner.fleetOwnerId
          driverId.getId
          (transporterConfig :: TC.TransporterConfig)
          incrementAcceptationCount
          incrementTotalRequestCount
          incrementRejectedRequestCount
          incrementPulledRequestCount
  where
    updateAcceptationCountRedisKey operatorId = do
      let acceptationCountKey = makeOperatorAnalyticsKey operatorId ACCEPTATION_COUNT
      ensureRedisKeysExistForAllTimeCommon transporterConfig DP.OPERATOR operatorId acceptationCountKey Redis.incrby 1
    updateTotalRequestCountRedisKey operatorId = do
      let totalRequestCountKey = makeOperatorAnalyticsKey operatorId TOTAL_REQUEST_COUNT
      ensureRedisKeysExistForAllTimeCommon transporterConfig DP.OPERATOR operatorId totalRequestCountKey Redis.incrby 1

-- | Batch update total request count for multiple drivers at once
-- This is more efficient than calling updateOperatorAnalyticsAcceptationTotalRequestAndPassedCount for each driver
updateOperatorAnalyticsTotalRequestCountBatch ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  [Id DP.Person] ->
  TC.TransporterConfig ->
  m ()
updateOperatorAnalyticsTotalRequestCountBatch driverIds transporterConfig = do
  -- Fetch all fleet associations for drivers in batch
  fleetAssociations <- QFDA.findAllByDriverIds driverIds
  let fleetDriverPairs = [(fa.fleetOwnerId, fa.driverId.getId) | fa <- fleetAssociations]

  -- Update fleet owner stats in batch (both overall and daily)
  unless (null fleetDriverPairs) $
    SFleetOperatorStats.incrementTotalRequestCountBatch fleetDriverPairs transporterConfig

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
  operatorIds <- findOperatorIdForDriver driverId
  when (null operatorIds) $ logTagInfo "AnalyticsUpdateRatingScoreKey" $ "No operator found for driver: " <> show driverId

  -- Operator analytics
  forM_ operatorIds $ \operatorId -> do
    let ratingSumKey = makeOperatorAnalyticsKey operatorId RATING_SUM
    let ratingCountKey = makeOperatorAnalyticsKey operatorId RATING_COUNT
    Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey operatorId) 10 5000 $
      SFleetOperatorStats.incrementTotalRatingCountAndTotalRatingScore operatorId transporterConfig ratingValue shouldIncrementCount
    -- Ensure Redis keys exist
    ensureRedisKeysExistForAllTimeCommon transporterConfig DP.OPERATOR operatorId ratingSumKey Redis.incrby (fromIntegral ratingValue)
    ensureRedisKeysExistForAllTimeCommon transporterConfig DP.OPERATOR operatorId ratingCountKey Redis.incrby (if shouldIncrementCount then 1 else 0)

  -- Fleet owner analytics
  mbFLeetOwner <- QFDA.findByDriverId driverId True
  when (isNothing mbFLeetOwner) $ logTagInfo "AnalyticsUpdateRatingScoreKey" $ "No fleet owner found for driver: " <> show driverId
  whenJust mbFLeetOwner $ \fleetOwner -> do
    Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwner.fleetOwnerId) 10 5000 $ do
      SFleetOperatorStats.incrementTotalRatingCountAndTotalRatingScore fleetOwner.fleetOwnerId transporterConfig ratingValue shouldIncrementCount
      SFleetOperatorStats.incrementTotalRatingCountAndTotalRatingScoreDaily fleetOwner.fleetOwnerId driverId.getId transporterConfig ratingValue shouldIncrementCount

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
  DBooking.Booking ->
  m ()
updateOperatorAnalyticsTotalRideCount transporterConfig driverId ride booking = do
  operatorIds <- findOperatorIdForDriver driverId
  when (null operatorIds) $ logTagInfo "AnalyticsUpdateTotalRideCount" $ "No operator found for driver: " <> show driverId
  forM_ operatorIds $ \operatorId -> do
    let totalRideCountKey = makeOperatorAnalyticsKey operatorId TOTAL_RIDE_COUNT
    Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey operatorId) 10 5000 $
      SFleetOperatorStats.incrementTotalRidesTotalDistAndTotalEarning operatorId ride transporterConfig
    -- Ensure Redis keys exist
    ensureRedisKeysExistForAllTimeCommon transporterConfig DP.OPERATOR operatorId totalRideCountKey Redis.incrby 1

  mbFLeetOwner <- QFDA.findByDriverId driverId True
  when (isNothing mbFLeetOwner) $ logTagInfo "AnalyticsUpdateTotalRideCount" $ "No fleet owner found for driver: " <> show driverId
  whenJust mbFLeetOwner $ \fleetOwner -> do
    Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwner.fleetOwnerId) 10 5000 $ do
      SFleetOperatorStats.incrementTotalRidesTotalDistAndTotalEarning fleetOwner.fleetOwnerId ride transporterConfig
      SFleetOperatorStats.incrementTotalEarningDistanceAndCompletedRidesDaily fleetOwner.fleetOwnerId ride booking transporterConfig

-- case newTotalRides of
--   2 -> updatePeriodicMetrics transporterConfig operatorId GREATER_THAN_ONE_RIDE Redis.incr
--   11 -> updatePeriodicMetrics transporterConfig operatorId GREATER_THAN_TEN_RIDE Redis.incr
--   51 -> updatePeriodicMetrics transporterConfig operatorId GREATER_THAN_FIFTY_RIDE Redis.incr
--   _ -> pure ()

updateEnabledVerifiedStateWithAnalytics :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, HasField "serviceClickhouseCfg" r CH.ClickhouseCfg, HasField "serviceClickhouseEnv" r CH.ClickhouseEnv) => Maybe DI.DriverInformation -> TC.TransporterConfig -> Id DP.Person -> Bool -> Maybe Bool -> m ()
updateEnabledVerifiedStateWithAnalytics mbDriverInfoData transporterConfig driverId isEnabled isVerified = do
  when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ do
    mbDriverInfo <- case mbDriverInfoData of
      Just driverInfoData -> pure (Just driverInfoData)
      Nothing -> QDI.findById driverId
    when (isNothing mbDriverInfo) $ logTagError "AnalyticsUpdateEnabledVerifiedState" $ "No driver info found for driver: " <> show driverId
    whenJust mbDriverInfo $ \di ->
      when (di.enabled /= isEnabled) $ do
        operatorIds <- findOperatorIdForDriver driverId
        let delta = if isEnabled then 1 :: Integer else -1
        forM_ operatorIds $ \oid ->
          adjustOperatorAllTimeAnalyticsMetric transporterConfig oid TOTAL_ENABLED_DRIVERS delta
  QDI.updateEnabledVerifiedState driverId isEnabled isVerified

incrementOperatorTotalActiveDriversIfFirstDriverSubscription ::
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
incrementOperatorTotalActiveDriversIfFirstDriverSubscription transporterConfig driverOwnerIdText =
  when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ do
    operatorIds <- findOperatorIdForDriver (Id driverOwnerIdText)
    unless (null operatorIds) $ do
      activeCount <- QSubscriptionPurchaseExtra.countActiveSubscriptionsForOwner driverOwnerIdText DSP.DRIVER
      when (activeCount == 0) $
        forM_ operatorIds $ \oid ->
          adjustOperatorAllTimeAnalyticsMetric transporterConfig oid TOTAL_ACTIVE_DRIVERS 1

-- Updation Logic of Fleet Owner Fields
incrementFleetOwnerAnalyticsActiveDriverCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Maybe Text ->
  Id DP.Person ->
  m ()
incrementFleetOwnerAnalyticsActiveDriverCount transporterConfig mbFleetOwnerId driverId = do
  case mbFleetOwnerId of
    Just fleetOwnerId -> incrementActiveDriverCount fleetOwnerId
    Nothing -> do
      mbFleetOwner <- QFDA.findByDriverId driverId True
      when (isNothing mbFleetOwner) $ logTagError "AnalyticsUpdateActiveDriverCount" $ "No fleet owner found for driver: " <> show driverId
      whenJust mbFleetOwner $ \fleetOwner -> incrementActiveDriverCount fleetOwner.fleetOwnerId
  where
    incrementActiveDriverCount fleetOwnerId = do
      let totalActiveDriverCountKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_DRIVER_COUNT
      ensureRedisKeysExistForAllTimeCommon transporterConfig DP.FLEET_OWNER fleetOwnerId totalActiveDriverCountKey Redis.incrby 1

decrementFleetOwnerAnalyticsActiveDriverCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Maybe Text ->
  Id DP.Person ->
  m ()
decrementFleetOwnerAnalyticsActiveDriverCount transporterConfig mbFleetOwnerId driverId = do
  case mbFleetOwnerId of
    Just fleetOwnerId -> decrementActiveDriverCount fleetOwnerId
    Nothing -> do
      mbFleetOwner <- QFDA.findByDriverId driverId True
      when (isNothing mbFleetOwner) $ logTagError "AnalyticsUpdateActiveDriverCount" $ "No fleet owner found for driver: " <> show driverId
      whenJust mbFleetOwner $ \fleetOwner -> decrementActiveDriverCount fleetOwner.fleetOwnerId
  where
    decrementActiveDriverCount fleetOwnerId = do
      let totalActiveDriverCountKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_DRIVER_COUNT
      ensureRedisKeysExistForAllTimeCommon transporterConfig DP.FLEET_OWNER fleetOwnerId totalActiveDriverCountKey Redis.decrby 1

incrementFleetOwnerAnalyticsActiveVehicleCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Maybe Text ->
  Id DP.Person ->
  m ()
incrementFleetOwnerAnalyticsActiveVehicleCount transporterConfig mbFleetOwnerId driverId = do
  when (isNothing mbFleetOwnerId) $ logTagError "AnalyticsUpdateActiveVehicleCount" $ "No fleet owner found for linked vehicle of driver: " <> show driverId
  whenJust mbFleetOwnerId $ \fleetOwnerId -> incrementActiveVehicleCount fleetOwnerId
  where
    incrementActiveVehicleCount fleetOwnerId = do
      let totalActiveVehicleCountKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_VEHICLE_COUNT
      ensureRedisKeysExistForAllTimeCommon transporterConfig DP.FLEET_OWNER fleetOwnerId totalActiveVehicleCountKey Redis.incrby 1

decrementFleetOwnerAnalyticsActiveVehicleCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Maybe Text ->
  Id DP.Person ->
  m ()
decrementFleetOwnerAnalyticsActiveVehicleCount transporterConfig mbFleetOwnerId driverId = do
  when (isNothing mbFleetOwnerId) $ logTagError "AnalyticsUpdateActiveVehicleCount" $ "No fleet owner found for linked vehicle of driver: " <> show driverId
  whenJust mbFleetOwnerId $ \fleetOwnerId -> decrementActiveVehicleCount fleetOwnerId
  where
    decrementActiveVehicleCount fleetOwnerId = do
      let totalActiveVehicleCountKey = makeFleetAnalyticsKey fleetOwnerId ACTIVE_VEHICLE_COUNT
      ensureRedisKeysExistForAllTimeCommon transporterConfig DP.FLEET_OWNER fleetOwnerId totalActiveVehicleCountKey Redis.decrby 1

-- | Compute period dashboard analytics via ClickHouse for a given operator and time window
computePeriodOperatorAnalytics ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  Day ->
  Day ->
  m PeriodRes
computePeriodOperatorAnalytics transporterConfig operatorId fromDay toDay = do
  let useDBForAnalytics = transporterConfig.analyticsConfig.useDbForEarningAndMetrics
  driverIds <- SDFStatus.getFleetDriverIdsAndDriverIdsByOperatorId operatorId
  activeDriver <- SDFStatus.getTotalFleetDriverAndDriverCountByOperatorIdInDateRange operatorId (UTCTime fromDay 0) (UTCTime toDay 86399)
  enabledCount <- CDI.getEnabledDriverCountByDriverIds driverIds (UTCTime fromDay 0) (UTCTime toDay 86399)
  -- gt1 <- CDaily.countDriversWithNumRidesGreaterThan1Between driverIds fromDay toDay
  -- gt10 <- CDaily.countDriversWithNumRidesGreaterThan10Between driverIds fromDay toDay
  -- gt50 <- CDaily.countDriversWithNumRidesGreaterThan50Between driverIds fromDay toDay

  (approvedDriverInspection, approvedVehicleInspection, rejectedDriverInspection, rejectedVehicleInspection) <- if useDBForAnalytics
    then QFleetOpsDailyExtra.sumApprovedVehicleAndDriverRequestsByFleetOperatorIdsAndDateRangeDB operatorId fromDay toDay
    else CFleetOpDailyStats.sumApprovedDriverAndVehicleRequestsByFleetOperatorIdsAndDateRange operatorId fromDay toDay

  pure PeriodRes {activeDriver, driverEnabled = enabledCount, greaterThanOneRide = 0 :: Int, greaterThanTenRide = 0 :: Int, greaterThanFiftyRide = 0 :: Int, approvedDriverInspection, approvedVehicleInspection, rejectedDriverInspection, rejectedVehicleInspection}

calculateDayNumberOfWeek :: Day -> Integer
calculateDayNumberOfWeek = fromIntegral . (\d -> d - 1) . fromEnum . dayOfWeek

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
