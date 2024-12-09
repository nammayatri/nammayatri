{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.BehaviourManagement.CancellationRate where

import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.DriverBlockTransactions as DTDBT
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.TransporterConfig
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import Lib.Scheduler.Environment
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import SharedLogic.External.LocationTrackingService.Types
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.Queries.DriverInformation as QDriverInformation
import Tools.Error
import Tools.Metrics (CoreMetrics)
import Tools.Notifications as Notify

data CancellationRateData = CancellationRateData
  { assignedCount :: Int,
    cancelledCount :: Int,
    cancellationRate :: Int,
    windowSize :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkRideAssignedKey :: Text -> Text
mkRideAssignedKey driverId = "driver-offer:CR:assigned-dId:" <> driverId

mkRideCancelledKey :: Text -> Text
mkRideCancelledKey driverId = "driver-offer:CR:cancelled-dId:" <> driverId

getWindowSize :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m Integer
getWindowSize mocId = do
  merchantConfig <- CTC.findByMerchantOpCityId mocId Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  pure $ toInteger $ fromMaybe 7 merchantConfig.cancellationRateWindow

incrementCancelledCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  Integer ->
  m ()
incrementCancelledCount driverId windowSize = Redis.withCrossAppRedis $ SWC.incrementWindowCount (mkRideCancelledKey driverId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)

incrementAssignedCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  Integer ->
  m ()
incrementAssignedCount driverId windowSize = Redis.withCrossAppRedis $ SWC.incrementWindowCount (mkRideAssignedKey driverId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)

getCancellationCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Integer ->
  Id DP.Person ->
  m Integer
getCancellationCount windowSize driverId = Redis.withCrossAppRedis $ SWC.getCurrentWindowCount (mkRideCancelledKey driverId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)

getAssignedCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Integer ->
  Id DP.Person ->
  m Integer
getAssignedCount windowSize driverId = Redis.withCrossAppRedis $ SWC.getCurrentWindowCount (mkRideAssignedKey driverId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)

getCancellationRateData ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m (Maybe CancellationRateData)
getCancellationRateData mocId driverId = do
  merchantConfig <- CTC.findByMerchantOpCityId mocId Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  let minimumRides = findMinimumRides merchantConfig
  let windowSize = findWindowSize merchantConfig
  assignedCount <- getAssignedCount windowSize driverId
  if (isJust merchantConfig.cancellationRateWindow) && (assignedCount > minimumRides)
    then do
      cancelledCount <- getCancellationCount windowSize driverId
      let cancellationRate = (cancelledCount * 100) `div` max 1 assignedCount
      pure $
        Just $
          CancellationRateData
            { assignedCount = fromInteger assignedCount,
              cancelledCount = fromInteger cancelledCount,
              cancellationRate = fromInteger cancellationRate,
              windowSize = fromMaybe 7 merchantConfig.cancellationRateWindow
            }
    else pure Nothing
  where
    findWindowSize merchantConfig = toInteger $ fromMaybe 7 merchantConfig.cancellationRateWindow
    findMinimumRides merchantConfig = toInteger $ fromMaybe 5 merchantConfig.cancellationRateCalculationThreshold

nudgeOrBlockDriver ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, CoreMetrics m, HasLocationService m r, JobCreator r m) =>
  TransporterConfig ->
  DP.Person ->
  DI.DriverInformation ->
  m ()
nudgeOrBlockDriver transporterConfig driver driverInfo = do
  let config = buildConfig
  now <- getCurrentTime
  case (transporterConfig.cancellationRateWindow, config) of
    (Just windowSize, Just CancellationRateBasedNudgingAndBlockingConfig {..}) -> do
      (dailyCancellationRate, dailyAssignedCount) <- getCancellationRateOfDays 1 windowSize
      (weeklyCancellationRate, weeklyAssignedCount) <- getCancellationRateOfDays 7 windowSize
      let mbCooldDownWeekly = driverInfo.weeklyCancellationRateBlockingCooldown
          mbCooldDownDaily = driverInfo.dailyCancellationRateBlockingCooldown
      blockedOnWeekly <- blockDriverCondition cancellationRateThresholdWeekly weeklyMinRidesforBlocking weeklyCancellationRate weeklyAssignedCount weeklyOffenceSuspensionTimeHours mbCooldDownWeekly now CancellationRateWeekly
      logDebug $ "All cancellation rate data: dailyCancellationRate: " <> show dailyCancellationRate <> " dailyAssignedCount: " <> show dailyAssignedCount <> " weeklyCancellationRate: " <> show weeklyCancellationRate <> " weeklyAssignedCount: " <> show weeklyAssignedCount
      if blockedOnWeekly
        then do
          let calculatedCooldownTime = addUTCTime (fromIntegral weeklyConditionCooldownTimeHours * 60 * 60) now
          let finalCooldown = whenFirstJustReturnMaxElseReturnSecond mbCooldDownWeekly calculatedCooldownTime
          QDriverInformation.updateWeeklyCancellationRateBlockingCooldown (Just finalCooldown) driver.id
        else do
          blockedOnDaily <- blockDriverCondition cancellationRateThresholdDaily dailyMinRidesforBlocking dailyCancellationRate dailyAssignedCount dailyOffenceSuspensionTimeHours mbCooldDownDaily now CancellationRateDaily
          if blockedOnDaily
            then do
              let calculatedCooldownTime = addUTCTime (fromIntegral dailyConditionCooldownTimeHours * 60 * 60) now
                  finalDailyCoolDown = whenFirstJustReturnMaxElseReturnSecond mbCooldDownDaily calculatedCooldownTime
                  finalyWeeklyCoolDown = whenFirstJustReturnMaxElseReturnSecond mbCooldDownWeekly calculatedCooldownTime
              QDriverInformation.updateDailyAndWeeklyCancellationRateBlockingCooldown (Just finalDailyCoolDown) (Just finalyWeeklyCoolDown) driver.id
            else do
              nudgeDriverCondition cancellationRateThresholdWeekly weeklyMinRidesforNudging weeklyMinRidesforBlocking weeklyCancellationRate weeklyAssignedCount FCM.CANCELLATION_RATE_NUDGE_WEEKLY "CANCELLATION_RATE_NUDGE_WEEKLY"
              nudgeDriverCondition cancellationRateThresholdDaily dailyMinRidesforNudging dailyMinRidesforBlocking dailyCancellationRate dailyAssignedCount FCM.CANCELLATION_RATE_NUDGE_DAILY "CANCELLATION_RATE_NUDGE_DAILY"
    _ -> logInfo "cancellationRateWindow or cancellationRateBasedNudgingAndBlockingConfig not found in transporter config"
  where
    whenFirstJustReturnMaxElseReturnSecond mbCooldown calculatedCooldown =
      case mbCooldown of
        Just cool -> max cool calculatedCooldown
        Nothing -> calculatedCooldown

    getCancellationRateOfDays period windowSize = do
      let windowInt = toInteger windowSize
      cancelledCount <- fmap (sum . map (fromMaybe 0)) $ Redis.withCrossAppRedis $ SWC.getCurrentWindowValuesUptoLast period (mkRideCancelledKey driver.id.getId) (SWC.SlidingWindowOptions windowInt SWC.Days)
      assignedCount <- fmap (sum . map (fromMaybe 0)) $ Redis.withCrossAppRedis $ SWC.getCurrentWindowValuesUptoLast period (mkRideAssignedKey driver.id.getId) (SWC.SlidingWindowOptions windowInt SWC.Days)
      let cancellationRate = (cancelledCount * 100) `div` max 1 assignedCount
      return (cancellationRate, assignedCount)

    blockDriverCondition :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, CoreMetrics m, HasLocationService m r, JobCreator r m) => Int -> Int -> Int -> Int -> Int -> Maybe UTCTime -> UTCTime -> BlockReasonFlag -> m Bool
    blockDriverCondition cancellationRateThreshold rideAssignedThreshold cancellationRate assignedCount blockTimeInHours mbCooldown now blockReasonFlag = do
      let rule = (cancellationRate > cancellationRateThreshold) && (assignedCount > rideAssignedThreshold)
      canApplyBlock <- case mbCooldown of
        Just cooldown -> return $ rule && (cooldown <= now)
        Nothing -> return rule
      if canApplyBlock
        then do
          logInfo $ "Blocking driver based on cancellation rate, driverId: " <> driver.id.getId
          QDriverInformation.updateDynamicBlockedStateWithActivity driver.id (Just "BLOCKED_BASED_ON_CANCELLATION_RATE") (Just blockTimeInHours) "AUTOMATICALLY_BLOCKED_BY_APP" driver.merchantId "AUTOMATICALLY_BLOCKED_BY_APP" driver.merchantOperatingCityId DTDBT.Application True (Just False) (Just DriverInfo.OFFLINE) blockReasonFlag
          let expiryTime = addUTCTime (fromIntegral blockTimeInHours * 60 * 60) now
          void $ LTS.blockDriverLocationsTill driver.merchantId driver.id expiryTime
          let unblockDriverJobTs = secondsToNominalDiffTime (fromIntegral blockTimeInHours) * 60 * 60
          JC.createJobIn @_ @'UnblockDriver (Just driver.merchantId) (Just driver.merchantOperatingCityId) unblockDriverJobTs $
            UnblockDriverRequestJobData
              { driverId = driver.id
              }
          return True
        else return False

    nudgeDriverCondition cancellationRateThreshold minAssignedRides maxAssignedRides cancellationRate rideAssignedCount fcmType pnKey = do
      let condition = (cancellationRate > cancellationRateThreshold) && (rideAssignedCount > minAssignedRides && rideAssignedCount <= maxAssignedRides)
      when condition $ do
        overlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory driver.merchantOperatingCityId pnKey (fromMaybe ENGLISH driver.language) Nothing Nothing >>= fromMaybeM (InternalError $ "Overlay not found for " <> pnKey)
        let fcmOverlayReq = Notify.mkOverlayReq overlay
        let entityData = Notify.CancellationRateBaseNudgeData {driverId = driver.id.getId, driverCancellationRate = cancellationRate}
        Notify.sendCancellationRateNudgeOverlay driver.merchantOperatingCityId driver fcmType fcmOverlayReq entityData

    buildConfig :: Maybe CancellationRateBasedNudgingAndBlockingConfig
    buildConfig = do
      cancellationRateThresholdDaily <- transporterConfig.cancellationRateThresholdDaily
      cancellationRateThresholdWeekly <- transporterConfig.cancellationRateThresholdWeekly
      dailyMinRidesforBlocking <- transporterConfig.dailyMinRidesForBlocking
      weeklyMinRidesforBlocking <- transporterConfig.weeklyMinRidesForBlocking
      dailyMinRidesforNudging <- transporterConfig.dailyMinRidesForNudging
      weeklyMinRidesforNudging <- transporterConfig.weeklyMinRidesForNudging
      dailyOffenceSuspensionTimeHours <- transporterConfig.dailyOffenceSuspensionTimeHours
      weeklyOffenceSuspensionTimeHours <- transporterConfig.weeklyOffenceSuspensionTimeHours
      dailyConditionCooldownTimeHours <- transporterConfig.dailyConditionCooldownTimeHours
      weeklyConditionCooldownTimeHours <- transporterConfig.weeklyConditionCooldownTimeHours
      return CancellationRateBasedNudgingAndBlockingConfig {..}
