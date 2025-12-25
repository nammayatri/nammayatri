{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module SharedLogic.BehaviourManagement.IssueBreachMitigation where

import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.DriverBlockTransactions as DTDBT
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Person as DP
import Domain.Types.ServiceTierType
import Domain.Types.TransporterConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import Lib.Scheduler.Environment
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator
import SharedLogic.BehaviourManagement.IssueBreach
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import SharedLogic.External.LocationTrackingService.Types
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.DriverInformation as QDriverInformation
import Tools.Error
import Tools.Metrics (CoreMetrics)
import Tools.Notifications

mkIssueBreachCounterKey :: IssueBreachType -> Text -> Text
mkIssueBreachCounterKey breachType driverId = "driver-offer:issue-breach:" <> show breachType <> "-counter-dId:" <> driverId

mkCompletedBookingCounterKey :: IssueBreachType -> Text -> Text
mkCompletedBookingCounterKey breachType driverId = "driver-offer:issue-breach:" <> show breachType <> ":completed-booking-counter-dId:" <> driverId

mkNotificationSendKey :: IssueBreachType -> Text -> Text
mkNotificationSendKey breachType driverId = "driver-offer:issue-breach:" <> show breachType <> ":pending-notify-entity:" <> driverId

incrementCompletedBookingCounterForIssueBreach ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  IssueBreachType ->
  Id DP.Person ->
  Integer ->
  m ()
incrementCompletedBookingCounterForIssueBreach breachType driverId windowSize = Redis.withCrossAppRedis $ SWC.incrementWindowCount (mkCompletedBookingCounterKey breachType driverId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)

incrementIssueBreachCounter ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  IssueBreachType ->
  Id DP.Person ->
  Integer ->
  m ()
incrementIssueBreachCounter breachType driverId windowSize = Redis.withCrossAppRedis $ SWC.incrementWindowCount (mkIssueBreachCounterKey breachType driverId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)

getIssueBreachCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  IssueBreachType ->
  Integer ->
  Id DP.Person ->
  m Integer
getIssueBreachCount breachType windowSize driverId = Redis.withCrossAppRedis $ SWC.getCurrentWindowCount (mkIssueBreachCounterKey breachType driverId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)

getIssueBreachConfig :: IssueBreachType -> TransporterConfig -> Maybe IssueBreachConfig
getIssueBreachConfig breachType transportConfig =
  find (\issueBreachConfig -> issueBreachConfig.ibIssueBreachType == breachType) (fromMaybe [] (transportConfig.issueBreachConfig))

getIssueBreachCooldownTime :: IssueBreachType -> DI.DriverInformation -> Maybe IssueBreachCooldownTime
getIssueBreachCooldownTime issueType driverInfo = do
  issueBreachCooldownTimes <- driverInfo.issueBreachCooldownTimes
  find (\issueBreachCooldownTime -> issueBreachCooldownTime.ibType == issueType) issueBreachCooldownTimes

updateIssueBreachCooldownTimes :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DI.DriverInformation -> IssueBreachCooldownTime -> m ()
updateIssueBreachCooldownTimes driverInfo cooldownTime = do
  let cooldownTimeType = ibType cooldownTime
      updateCooldownTimes = (fromMaybe [] driverInfo.issueBreachCooldownTimes) & filter (\issueBreachCooldownTime -> ibType issueBreachCooldownTime /= cooldownTimeType) & (cooldownTime :)
  QDriverInformation.updateIssueBreachCooldownTimes (pure updateCooldownTimes) driverInfo.driverId

issueBreachMitigation ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, CoreMetrics m, HasLocationService m r, JobCreator r m, HasShortDurationRetryCfg r c) =>
  IssueBreachType ->
  TransporterConfig ->
  DI.DriverInformation ->
  m ()
issueBreachMitigation issueType transporterConfig driverInfo = when (isJust transporterConfig.issueBreachConfig && not driverInfo.blocked) $ do
  let issueBreachConfigOfIssueType = getIssueBreachConfig issueType transporterConfig
  whenJust issueBreachConfigOfIssueType $ \config -> do
    -- checking if notification not send and sending if ride finished --
    when (config.ibBlockType == IBSoft && (isJust driverInfo.softBlockStiers) && not driverInfo.onRide) $ do
      let isNotificationPendingKey = mkNotificationSendKey issueType driverInfo.driverId.getId
      mbPendingIssueBreachNotificationEntity <- Redis.get isNotificationPendingKey
      whenJust mbPendingIssueBreachNotificationEntity $ \entity -> do
        let notifyDriverJobTs = secondsToNominalDiffTime (fromIntegral config.ibNotifyInMins) * 60
        JC.createJobIn @_ @'SoftBlockNotifyDriver (Just transporterConfig.merchantId) (Just transporterConfig.merchantOperatingCityId) notifyDriverJobTs $
          SoftBlockNotifyDriverRequestJobData
            { driverId = driverInfo.driverId,
              pendingNotificationRedisKey = isNotificationPendingKey,
              entityData = entity
            }
        Redis.del isNotificationPendingKey
    when ((config.ibBlockType == IBHard && not driverInfo.onRide) || (config.ibBlockType == IBSoft && (isNothing driverInfo.softBlockStiers))) $ do
      now <- getCurrentTime
      let (blockReasonDaily, blockReasonWeekly) = getBlockReasonFlag issueType
      let windowSize = toInteger config.ibCountWindowSizeInDays
      (issueBreachRateDaily, completedBookingCountDaily) <- getIssueBreachCountOfDays 1 windowSize
      (issueBreachRateWeekly, completedBookingCountWeekly) <- getIssueBreachCountOfDays 7 windowSize
      let issueBreachCooldownTime = getIssueBreachCooldownTime issueType driverInfo
          mbCooldDownWeekly = issueBreachCooldownTime >>= (.ibWeeklyCooldownTimeInHours)
          mbCooldDownDaily = issueBreachCooldownTime >>= (.ibDailyCooldownTimeInHours)
      blockedOnWeekly <- tryBlockDriver (config.ibNotifyInMins) (config.ibAllowedServiceTiers) (config.ibBlockType) (config.ibRateThresholdWeekly) (config.ibWeeklyMinRidesforBlocking) issueBreachRateWeekly completedBookingCountWeekly (config.ibWeeklyOffenceSuspensionTimeInHours) mbCooldDownWeekly now blockReasonWeekly
      if blockedOnWeekly
        then do
          let calculatedCooldownTime = addUTCTime (fromIntegral config.ibWeeklyCooldownTimeInHours * 60 * 60) now
              finalWeeklyCooldownTime = calculateFinalCoolDownTime mbCooldDownWeekly calculatedCooldownTime
              finalCooldownTime = IssueBreachCooldownTime {ibWeeklyCooldownTimeInHours = Just finalWeeklyCooldownTime, ibDailyCooldownTimeInHours = mbCooldDownDaily, ibType = issueType}
          updateIssueBreachCooldownTimes driverInfo finalCooldownTime
        else do
          blockedOnDaily <- tryBlockDriver (config.ibNotifyInMins) (config.ibAllowedServiceTiers) (config.ibBlockType) (config.ibRateThresholdDaily) (config.ibDailyMinRidesforBlocking) issueBreachRateDaily completedBookingCountDaily (config.ibDailyOffenceSuspensionTimeInHours) mbCooldDownDaily now blockReasonDaily
          when blockedOnDaily $ do
            let calculatedCooldownTime = addUTCTime (fromIntegral config.ibDailyCooldownTimeInHours * 60 * 60) now
                dailyFinalCooldownTime = calculateFinalCoolDownTime mbCooldDownDaily calculatedCooldownTime
                weeklyFinalCooldownTime = calculateFinalCoolDownTime mbCooldDownWeekly calculatedCooldownTime
                finalCooldownTime = IssueBreachCooldownTime {ibDailyCooldownTimeInHours = Just dailyFinalCooldownTime, ibWeeklyCooldownTimeInHours = Just weeklyFinalCooldownTime, ibType = issueType}
            updateIssueBreachCooldownTimes driverInfo finalCooldownTime
  where
    calculateFinalCoolDownTime :: Maybe UTCTime -> UTCTime -> UTCTime
    calculateFinalCoolDownTime prevCooldownTime calculatedCooldownTime =
      case prevCooldownTime of
        Just prevTime -> max prevTime calculatedCooldownTime
        Nothing -> calculatedCooldownTime

    getIssueBreachCountOfDays period windowSize = do
      ibCount <- fmap (sum . map (fromMaybe 0)) $ Redis.withCrossAppRedis $ SWC.getCurrentWindowValuesUptoLast period (mkIssueBreachCounterKey issueType driverInfo.driverId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)
      completedBookingCount <- fmap (sum . map (fromMaybe 0)) $ Redis.withCrossAppRedis $ SWC.getCurrentWindowValuesUptoLast period (mkCompletedBookingCounterKey issueType driverInfo.driverId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)
      let ibRate = (ibCount * 100) `div` max 1 completedBookingCount
      return (ibRate, completedBookingCount)

    tryBlockDriver :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, CoreMetrics m, HasLocationService m r, JobCreator r m, HasShortDurationRetryCfg r c) => Int -> [ServiceTierType] -> IssueBreachBlockType -> Int -> Int -> Int -> Int -> Int -> Maybe UTCTime -> UTCTime -> BlockReasonFlag -> m Bool
    tryBlockDriver ibNotifyInMins allowedSTiers ibBlockType issueBreachRateThreshold minCompletedBookingsRequired ibRate completedBookingCount blockTimeInHours cooldownTime now blockReasonFlag = do
      let rule = ibRate >= issueBreachRateThreshold && completedBookingCount >= minCompletedBookingsRequired
          canApplyBlock = rule && maybe True (<= now) cooldownTime
      when canApplyBlock $ do
        let expiryTime = addUTCTime (fromIntegral blockTimeInHours * 60 * 60) now
        let unblockDriverJobTs = secondsToNominalDiffTime (fromIntegral blockTimeInHours) * 60 * 60
            notifyDriverJobTs = secondsToNominalDiffTime (fromIntegral ibNotifyInMins) * 60
        case ibBlockType of
          IBSoft -> do
            logInfo $ "Soft Blocking driver " <> driverInfo.driverId.getId <> " due to issue breach rate " <> show ibRate <> " and completed booking count " <> show completedBookingCount <> ". Reason: " <> show blockReasonFlag
            QDriverInformation.updateSoftBlock (Just allowedSTiers) (Just expiryTime) (Just $ show blockReasonFlag) driverInfo.driverId
            JC.createJobIn @_ @'UnblockSoftBlockedDriver (Just transporterConfig.merchantId) (Just transporterConfig.merchantOperatingCityId) unblockDriverJobTs $
              UnblockSoftBlockedDriverRequestJobData
                { driverId = driverInfo.driverId
                }
            let notificationEntityData =
                  IssueBreachEntityData
                    { ibName = "ISSUE_BREACH_" <> show issueType,
                      blockExpirationTime = expiryTime,
                      blockedReasonFlag = show blockReasonFlag,
                      blockedSTiers = allowedSTiers
                    }
            let isNotificationPendingKey = mkNotificationSendKey issueType driverInfo.driverId.getId
            if not driverInfo.onRide
              then do
                void $
                  JC.createJobIn @_ @'SoftBlockNotifyDriver (Just transporterConfig.merchantId) (Just transporterConfig.merchantOperatingCityId) notifyDriverJobTs $
                    SoftBlockNotifyDriverRequestJobData
                      { driverId = driverInfo.driverId,
                        pendingNotificationRedisKey = isNotificationPendingKey,
                        entityData = notificationEntityData
                      }
              else do
                void $ Redis.setExp isNotificationPendingKey notificationEntityData (blockTimeInHours * 60 * 60)
          IBHard -> do
            logInfo $ "Blocking driver " <> driverInfo.driverId.getId <> " due to issue breach rate " <> show ibRate <> " and completed booking count " <> show completedBookingCount <> ". Reason: " <> show blockReasonFlag
            QDriverInformation.updateDynamicBlockedStateWithActivity driverInfo.driverId (Just $ "ISSUE_BREACH_" <> show issueType) (Just blockTimeInHours) "AUTOMATICALLY_BLOCKED_BY_APP" transporterConfig.merchantId "AUTOMATICALLY_BLOCKED_BY_APP" transporterConfig.merchantOperatingCityId DTDBT.Application True (Just False) (Just DriverInfo.OFFLINE) blockReasonFlag
            void $ LTS.blockDriverLocationsTill transporterConfig.merchantId driverInfo.driverId expiryTime
            JC.createJobIn @_ @'UnblockDriver (Just transporterConfig.merchantId) (Just transporterConfig.merchantOperatingCityId) unblockDriverJobTs $
              UnblockDriverRequestJobData
                { driverId = driverInfo.driverId
                }
      return canApplyBlock

getBlockReasonFlag :: IssueBreachType -> (BlockReasonFlag, BlockReasonFlag)
getBlockReasonFlag issueType =
  case issueType of
    EXTRA_FARE_MITIGATION -> (ExtraFareDaily, ExtraFareWeekly)
