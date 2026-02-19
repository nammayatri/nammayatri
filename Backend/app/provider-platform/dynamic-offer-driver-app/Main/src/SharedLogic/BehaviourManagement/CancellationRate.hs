{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.BehaviourManagement.CancellationRate where

import Data.Time (UTCTime (..), utctDay)
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
import qualified Lib.Yudhishthira.Flow.Dashboard as YudhishthiraFlow
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.External.LocationTrackingService.Types
import qualified SharedLogic.Person as SPerson
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import Tools.Metrics (CoreMetrics)
import Tools.Notifications as Notify

data CancellationRateData = CancellationRateData
  { assignedCount :: Int,
    cancelledCount :: Int,
    cancellationRate :: Int,
    windowSize :: Int,
    assignedCountDaily :: Int,
    cancelledCountDaily :: Int,
    assignedCountWeekly :: Int,
    cancelledCountWeekly :: Int
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

      (cancelledCountDaily, assignedCountDaily) <- getCancellationRateOfDaysStandalone driverId 1 windowSize
      (cancelledCountWeekly, assignedCountWeekly) <- getCancellationRateOfDaysStandalone driverId 7 windowSize
      pure $
        Just $
          CancellationRateData
            { assignedCount = fromInteger assignedCount,
              cancelledCount = fromInteger cancelledCount,
              cancellationRate = fromInteger cancellationRate,
              windowSize = fromMaybe 7 merchantConfig.cancellationRateWindow,
              assignedCountDaily = fromInteger assignedCountDaily,
              cancelledCountDaily = fromInteger cancelledCountDaily,
              assignedCountWeekly = fromInteger assignedCountWeekly,
              cancelledCountWeekly = fromInteger cancelledCountWeekly
            }
    else pure Nothing
  where
    findWindowSize merchantConfig = toInteger $ fromMaybe 7 merchantConfig.cancellationRateWindow
    findMinimumRides merchantConfig = toInteger $ fromMaybe 5 merchantConfig.cancellationRateCalculationThreshold

getCancellationRateOfDaysStandalone ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Integer ->
  Integer ->
  m (Integer, Integer)
getCancellationRateOfDaysStandalone driverId period windowSize = do
  cancelledCount <- fmap (sum . map (fromMaybe 0)) $ Redis.withCrossAppRedis $ SWC.getCurrentWindowValuesUptoLast period (mkRideCancelledKey driverId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)
  assignedCount <- fmap (sum . map (fromMaybe 0)) $ Redis.withCrossAppRedis $ SWC.getCurrentWindowValuesUptoLast period (mkRideAssignedKey driverId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)
  return (cancelledCount, assignedCount)

-- Update driver cancellation percentage tags once per day
-- Uses Redis to track last update time to ensure it runs only once per day per driver
-- Takes pre-calculated cancellation rates to avoid redundant calculations
updateDriverCancellationPercentageTagsDaily ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Int ->
  Int ->
  m ()
updateDriverCancellationPercentageTagsDaily _mocId driverId dailyCancellationRate weeklyCancellationRate = do
  now <- getCurrentTime
  let today = utctDay now
      redisKey = "driver-cancellation-tags-updated:" <> driverId.getId <> ":" <> show today

  -- Check if already updated today
  alreadyUpdated <- Redis.withCrossAppRedis $ Redis.get @Text redisKey
  when (isNothing alreadyUpdated) $ do
    -- Update tags using pre-calculated rates
    driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    let dailyTag = Yudhishthira.mkTagNameValue (LYT.TagName "driver_cancellation_1") (LYT.NumberValue $ fromIntegral dailyCancellationRate)
        weeklyTag = Yudhishthira.mkTagNameValue (LYT.TagName "driver_cancellation_7") (LYT.NumberValue $ fromIntegral weeklyCancellationRate)

    mbDailyTag <- catch (YudhishthiraFlow.verifyTag dailyTag) (\(_ :: SomeException) -> pure Nothing)
    mbWeeklyTag <- catch (YudhishthiraFlow.verifyTag weeklyTag) (\(_ :: SomeException) -> pure Nothing)

    let dailyTagWithExpiry = Yudhishthira.addTagExpiry dailyTag (mbDailyTag >>= \tag -> tag.validity) now
        weeklyTagWithExpiry = Yudhishthira.addTagExpiry weeklyTag (mbWeeklyTag >>= \tag -> tag.validity) now
        updatedTags = Yudhishthira.replaceTagNameValue (Just $ Yudhishthira.replaceTagNameValue driver.driverTag dailyTagWithExpiry) weeklyTagWithExpiry

    unless (Just (Yudhishthira.showRawTags updatedTags) == (Yudhishthira.showRawTags <$> driver.driverTag)) $ do
      QPerson.updateDriverTag (Just updatedTags) driverId
      -- Mark as updated today (expires at end of day)
      let secondsUntilMidnight = diffUTCTime (addUTCTime (fromIntegral (24 * 60 * 60 :: Int)) (UTCTime today 0)) now
      Redis.withCrossAppRedis $ Redis.set redisKey ("1" :: Text) >> Redis.expire redisKey (round secondsUntilMidnight)
      logDebug $ "Updated cancellation percentage tags for driver " <> driverId.getId

nudgeOrBlockDriver ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, CoreMetrics m, HasLocationService m r, JobCreator r m, HasShortDurationRetryCfg r c) =>
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
      logDebug $ "All cancellation rate data for driverId: " <> driver.id.getId <> ": dailyCancellationRate: " <> show dailyCancellationRate <> " dailyAssignedCount: " <> show dailyAssignedCount <> " weeklyCancellationRate: " <> show weeklyCancellationRate <> " weeklyAssignedCount: " <> show weeklyAssignedCount <> "cancellationrateslabconfig" <> show cancellationRateSlabConfig

      (canBlockOnWeekly, canBlockOnDaily, weeklyOffenceSuspensionTimeHoursParam, dailyOffenceSuspensionTimeHoursParam) <- case cancellationRateSlabConfig of
        Nothing -> do
          return
            ( isDriverBlockable cancellationRateThresholdWeekly weeklyMinRidesforBlocking weeklyCancellationRate weeklyAssignedCount mbCooldDownWeekly now,
              isDriverBlockable cancellationRateThresholdDaily dailyMinRidesforBlocking dailyCancellationRate dailyAssignedCount mbCooldDownDaily now,
              weeklyOffenceSuspensionTimeHours,
              dailyOffenceSuspensionTimeHours
            )
        Just configData -> do
          let matchingWeeklySlab = find (\slab -> isDriverBlockableSlab (slab.penalityForCancellation.cancellationPercentageThreshold) (slab.minBookingsRange) weeklyCancellationRate weeklyAssignedCount slab.penalityForCancellation.suspensionTimeInHours mbCooldDownWeekly now) (configData.weeklySlabs)
          (canBlockOnWeekly', weeklyOffenceSuspensionTimeHoursParam') <- case matchingWeeklySlab of
            Just slab -> return (True, slab.penalityForCancellation.suspensionTimeInHours)
            Nothing -> return (False, weeklyOffenceSuspensionTimeHours)

          let matchingDailySlab =
                find
                  ( \slab ->
                      isDriverBlockableSlab
                        slab.penalityForCancellation.cancellationPercentageThreshold
                        slab.minBookingsRange
                        dailyCancellationRate
                        dailyAssignedCount
                        slab.penalityForCancellation.suspensionTimeInHours
                        mbCooldDownDaily
                        now
                  )
                  configData.dailySlabs

          (canBlockOnDaily', dailyOffenceSuspensionTimeHoursParam') <- case matchingDailySlab of
            Just slab -> return (True, slab.penalityForCancellation.suspensionTimeInHours)
            Nothing -> return (False, dailyOffenceSuspensionTimeHours)

          return (canBlockOnWeekly', canBlockOnDaily', weeklyOffenceSuspensionTimeHoursParam', dailyOffenceSuspensionTimeHoursParam')
      case (canBlockOnWeekly, canBlockOnDaily, weeklyOffenceSuspensionTimeHoursParam, dailyOffenceSuspensionTimeHoursParam) of
        (True, _, weeklyOffenceSuspensionTimeHoursParam', _) -> do
          SPerson.blockDriverTemporarily driver.merchantId driver.merchantOperatingCityId driver.id "BLOCKED_BASED_ON_CANCELLATION_RATE" weeklyOffenceSuspensionTimeHoursParam' CancellationRateWeekly
          let calculatedCooldownTime = addUTCTime (fromIntegral weeklyConditionCooldownTimeHours * 60 * 60) now
              finalCooldown = whenFirstJustReturnMaxElseReturnSecond mbCooldDownWeekly calculatedCooldownTime
          QDriverInformation.updateWeeklyCancellationRateBlockingCooldown (Just finalCooldown) driver.id
        (_, True, _, dailyOffenceSuspensionTimeHoursParam') -> do
          SPerson.blockDriverTemporarily driver.merchantId driver.merchantOperatingCityId driver.id "BLOCKED_BASED_ON_CANCELLATION_RATE" dailyOffenceSuspensionTimeHoursParam' CancellationRateDaily
          let calculatedCooldownTime = addUTCTime (fromIntegral dailyConditionCooldownTimeHours * 60 * 60) now
              finalDailyCoolDown = whenFirstJustReturnMaxElseReturnSecond mbCooldDownDaily calculatedCooldownTime
              finalyWeeklyCoolDown = whenFirstJustReturnMaxElseReturnSecond mbCooldDownWeekly calculatedCooldownTime
          QDriverInformation.updateDailyAndWeeklyCancellationRateBlockingCooldown (Just finalDailyCoolDown) (Just finalyWeeklyCoolDown) driver.id
        _ -> do
          when (canNudgeDriver cancellationRateThresholdWeekly weeklyMinRidesforNudging weeklyMinRidesforBlocking weeklyCancellationRate weeklyAssignedCount) $ do
            nudgeDriver weeklyCancellationRate FCM.CANCELLATION_RATE_NUDGE_WEEKLY "CANCELLATION_RATE_NUDGE_WEEKLY"
          when (canNudgeDriver cancellationRateThresholdDaily dailyMinRidesforNudging dailyMinRidesforBlocking dailyCancellationRate dailyAssignedCount) $ do
            nudgeDriver dailyCancellationRate FCM.CANCELLATION_RATE_NUDGE_DAILY "CANCELLATION_RATE_NUDGE_DAILY"

      -- Update driver tags with cancellation percentages once per day (uses Redis to ensure once per day per driver)
      -- Uses already-calculated rates to avoid redundant queries
      fork "Update cancellation percentage tags daily" $ do
        catch (updateDriverCancellationPercentageTagsDaily driver.merchantOperatingCityId driver.id dailyCancellationRate weeklyCancellationRate) $ \(err :: SomeException) -> do
          logError $ "Failed to update cancellation percentage tags for driver " <> driver.id.getId <> ": " <> show err
    _ -> logInfo "cancellationRateWindow or cancellationRateBasedNudgingAndBlockingConfig not found in transporter config"
  where
    canNudgeDriver cancellationRateThreshold minAssignedRides maxAssignedRides cancellationRate rideAssignedCount =
      (cancellationRate > cancellationRateThreshold) && (rideAssignedCount > minAssignedRides && rideAssignedCount <= maxAssignedRides)

    whenFirstJustReturnMaxElseReturnSecond mbCooldown calculatedCooldown =
      case mbCooldown of
        Just cool -> max cool calculatedCooldown
        Nothing -> calculatedCooldown

    isDriverBlockable cancellationRateThreshold rideAssignedThreshold cancellationRate assignedCount mbCooldown now =
      let rule = (cancellationRate >= cancellationRateThreshold) && (assignedCount >= rideAssignedThreshold)
       in case mbCooldown of
            Just cooldown -> rule && (cooldown <= now)
            Nothing -> rule

    isDriverBlockableSlab cancellationRateThreshold rideAssignedThreshold cancellationRate assignedCount suspensionTimeInHours mbCooldown now =
      let rule = maybe False (\minRides -> maybe False (\maxRides -> (cancellationRate >= cancellationRateThreshold) && (assignedCount >= minRides) && (assignedCount <= maxRides) && (suspensionTimeInHours > 0)) (listToMaybe (reverse rideAssignedThreshold))) (listToMaybe rideAssignedThreshold)
       in case mbCooldown of
            Just cooldown -> rule && (cooldown <= now)
            Nothing -> rule

    getCancellationRateOfDays period windowSize = do
      let windowInt = toInteger windowSize
      cancelledCount <- fmap (sum . map (fromMaybe 0)) $ Redis.withCrossAppRedis $ SWC.getCurrentWindowValuesUptoLast period (mkRideCancelledKey driver.id.getId) (SWC.SlidingWindowOptions windowInt SWC.Days)
      assignedCount <- fmap (sum . map (fromMaybe 0)) $ Redis.withCrossAppRedis $ SWC.getCurrentWindowValuesUptoLast period (mkRideAssignedKey driver.id.getId) (SWC.SlidingWindowOptions windowInt SWC.Days)
      let cancellationRate = ((cancelledCount) * 100) `div` max 1 (assignedCount)
      return (cancellationRate, assignedCount)

    nudgeDriver cancellationRate fcmType pnKey = do
      overlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory driver.merchantOperatingCityId pnKey (fromMaybe ENGLISH driver.language) Nothing Nothing Nothing >>= fromMaybeM (InternalError $ "Overlay not found for " <> pnKey)
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
      let cancellationRateSlabConfig = transporterConfig.cancellationRateSlabConfig
      return CancellationRateBasedNudgingAndBlockingConfig {..}
