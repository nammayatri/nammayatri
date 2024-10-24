{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.BehaviourManagement.CancellationRate where

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
import SharedLogic.External.LocationTrackingService.Types
import qualified SharedLogic.Person as SPerson
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
      logDebug $ "All cancellation rate data for driverId: " <> driver.id.getId <> ": dailyCancellationRate: " <> show dailyCancellationRate <> " dailyAssignedCount: " <> show dailyAssignedCount <> " weeklyCancellationRate: " <> show weeklyCancellationRate <> " weeklyAssignedCount: " <> show weeklyAssignedCount
      let canBlockOnWeekly = isDriverBlockable cancellationRateThresholdWeekly weeklyMinRidesforBlocking weeklyCancellationRate weeklyAssignedCount mbCooldDownWeekly now
          canBlockOnDaily = isDriverBlockable cancellationRateThresholdDaily dailyMinRidesforBlocking dailyCancellationRate dailyAssignedCount mbCooldDownDaily now
      case (canBlockOnWeekly, canBlockOnDaily) of
        (True, _) -> do
          SPerson.blockDriverTemporarily driver.merchantId driver.merchantOperatingCityId driver.id "BLOCKED_BASED_ON_CANCELLATION_RATE" weeklyOffenceSuspensionTimeHours CancellationRateWeekly
          let calculatedCooldownTime = addUTCTime (fromIntegral weeklyConditionCooldownTimeHours * 60 * 60) now
              finalCooldown = whenFirstJustReturnMaxElseReturnSecond mbCooldDownWeekly calculatedCooldownTime
          QDriverInformation.updateWeeklyCancellationRateBlockingCooldown (Just finalCooldown) driver.id
        (_, True) -> do
          SPerson.blockDriverTemporarily driver.merchantId driver.merchantOperatingCityId driver.id "BLOCKED_BASED_ON_CANCELLATION_RATE" dailyOffenceSuspensionTimeHours CancellationRateDaily
          let calculatedCooldownTime = addUTCTime (fromIntegral dailyConditionCooldownTimeHours * 60 * 60) now
              finalDailyCoolDown = whenFirstJustReturnMaxElseReturnSecond mbCooldDownDaily calculatedCooldownTime
              finalyWeeklyCoolDown = whenFirstJustReturnMaxElseReturnSecond mbCooldDownWeekly calculatedCooldownTime
          QDriverInformation.updateDailyAndWeeklyCancellationRateBlockingCooldown (Just finalDailyCoolDown) (Just finalyWeeklyCoolDown) driver.id
        _ -> do
          when (canNudgeDriver cancellationRateThresholdWeekly weeklyMinRidesforNudging weeklyMinRidesforBlocking weeklyCancellationRate weeklyAssignedCount) $ do
            nudgeDriver weeklyCancellationRate FCM.CANCELLATION_RATE_NUDGE_WEEKLY "CANCELLATION_RATE_NUDGE_WEEKLY"
          when (canNudgeDriver cancellationRateThresholdDaily dailyMinRidesforNudging dailyMinRidesforBlocking dailyCancellationRate dailyAssignedCount) $ do
            nudgeDriver dailyCancellationRate FCM.CANCELLATION_RATE_NUDGE_DAILY "CANCELLATION_RATE_NUDGE_DAILY"
    _ -> logInfo "cancellationRateWindow or cancellationRateBasedNudgingAndBlockingConfig not found in transporter config"
  where
    canNudgeDriver cancellationRateThreshold minAssignedRides maxAssignedRides cancellationRate rideAssignedCount =
      (cancellationRate > cancellationRateThreshold) && (rideAssignedCount > minAssignedRides && rideAssignedCount <= maxAssignedRides)

    whenFirstJustReturnMaxElseReturnSecond mbCooldown calculatedCooldown =
      case mbCooldown of
        Just cool -> max cool calculatedCooldown
        Nothing -> calculatedCooldown

    isDriverBlockable cancellationRateThreshold rideAssignedThreshold cancellationRate assignedCount mbCooldown now = do
      let rule = (cancellationRate > cancellationRateThreshold) && (assignedCount > rideAssignedThreshold)
      case mbCooldown of
        Just cooldown -> rule && (cooldown <= now)
        Nothing -> rule

    getCancellationRateOfDays period windowSize = do
      let windowInt = toInteger windowSize
      cancelledCount <- fmap (sum . map (fromMaybe 0)) $ Redis.withCrossAppRedis $ SWC.getCurrentWindowValuesUptoLast period (mkRideCancelledKey driver.id.getId) (SWC.SlidingWindowOptions windowInt SWC.Days)
      assignedCount <- fmap (sum . map (fromMaybe 0)) $ Redis.withCrossAppRedis $ SWC.getCurrentWindowValuesUptoLast period (mkRideAssignedKey driver.id.getId) (SWC.SlidingWindowOptions windowInt SWC.Days)
      let cancellationRate = (cancelledCount * 100) `div` max 1 assignedCount
      return (cancellationRate, assignedCount)

    nudgeDriver cancellationRate fcmType pnKey = do
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
