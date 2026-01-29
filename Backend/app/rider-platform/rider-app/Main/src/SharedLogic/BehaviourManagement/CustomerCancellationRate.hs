{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.BehaviourManagement.CustomerCancellationRate where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.RiderConfig as RC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import Lib.Scheduler.Environment
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import qualified SharedLogic.JobScheduler as RJS
import qualified Storage.CachedQueries.Merchant.RiderConfig as CRC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.PersonExtra as QPExtra
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.BookingExtra as QBExtra
import Tools.Error

data CustomerCancellationRateData = CustomerCancellationRateData
  { assignedCount :: Int,
    cancelledCount :: Int,
    cancellationRate :: Int,
    windowSize :: Int,
    assignedCountDaily :: Int,
    cancelledCountDaily :: Int,
    assignedCountWeekly :: Int,
    cancelledCountWeekly :: Int,
    assignedCountMonthly :: Int,
    cancelledCountMonthly :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkRideAssignedKey :: Text -> Text
mkRideAssignedKey customerId = "customer-offer:CR:assigned-cId:" <> customerId

mkRideCancelledKey :: Text -> Text
mkRideCancelledKey customerId = "customer-offer:CR:cancelled-cId:" <> customerId

getWindowSize :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m Integer
getWindowSize mocId = do
  riderConfig <- CRC.findByMerchantOperatingCityId mocId Nothing >>= fromMaybeM (RiderConfigDoesNotExist mocId.getId)
  pure $ toInteger $ fromMaybe 30 riderConfig.cancellationRateWindow

incrementCancelledCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  Integer ->
  m ()
incrementCancelledCount customerId windowSize = Redis.withCrossAppRedis $ SWC.incrementWindowCount (mkRideCancelledKey customerId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)

incrementAssignedCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  Integer ->
  m ()
incrementAssignedCount customerId windowSize = Redis.withCrossAppRedis $ SWC.incrementWindowCount (mkRideAssignedKey customerId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)

getCancellationCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Integer ->
  Id DP.Person ->
  m Integer
getCancellationCount windowSize customerId = Redis.withCrossAppRedis $ SWC.getCurrentWindowCount (mkRideCancelledKey customerId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)

getAssignedCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Integer ->
  Id DP.Person ->
  m Integer
getAssignedCount windowSize customerId = Redis.withCrossAppRedis $ SWC.getCurrentWindowCount (mkRideAssignedKey customerId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)

getCancellationRateData ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m (Maybe CustomerCancellationRateData)
getCancellationRateData mocId customerId = do
  riderConfig <- CRC.findByMerchantOperatingCityId mocId Nothing >>= fromMaybeM (RiderConfigDoesNotExist mocId.getId)
  let minimumRides = findMinimumRides riderConfig
  let windowSize = findWindowSize riderConfig
  assignedCount <- getAssignedCount windowSize customerId
  if (isJust riderConfig.cancellationRateWindow) && (assignedCount > minimumRides)
    then do
      cancelledCount <- getCancellationCount windowSize customerId
      let cancellationRate = (cancelledCount * 100) `div` max 1 assignedCount

      (cancelledCountDaily, assignedCountDaily) <- getCancellationRateOfDaysStandalone customerId 1 windowSize
      (cancelledCountWeekly, assignedCountWeekly) <- getCancellationRateOfDaysStandalone customerId 7 windowSize
      (cancelledCountMonthly, assignedCountMonthly) <- getCancellationRateOfDaysStandalone customerId 30 windowSize
      pure $
        Just $
          CustomerCancellationRateData
            { assignedCount = fromInteger assignedCount,
              cancelledCount = fromInteger cancelledCount,
              cancellationRate = fromInteger cancellationRate,
              windowSize = fromMaybe 30 riderConfig.cancellationRateWindow,
              assignedCountDaily = fromInteger assignedCountDaily,
              cancelledCountDaily = fromInteger cancelledCountDaily,
              assignedCountWeekly = fromInteger assignedCountWeekly,
              cancelledCountWeekly = fromInteger cancelledCountWeekly,
              assignedCountMonthly = fromInteger assignedCountMonthly,
              cancelledCountMonthly = fromInteger cancelledCountMonthly
            }
    else pure Nothing
  where
    findWindowSize riderConfig = toInteger $ fromMaybe 30 riderConfig.cancellationRateWindow
    findMinimumRides riderConfig = toInteger $ fromMaybe 5 riderConfig.cancellationRateCalculationThreshold

getCancellationRateOfDaysStandalone ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Integer ->
  Integer ->
  m (Integer, Integer)
getCancellationRateOfDaysStandalone customerId period windowSize = do
  cancelledCount <- fmap (sum . map (fromMaybe 0)) $ Redis.withCrossAppRedis $ SWC.getCurrentWindowValuesUptoLast period (mkRideCancelledKey customerId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)
  assignedCount <- fmap (sum . map (fromMaybe 0)) $ Redis.withCrossAppRedis $ SWC.getCurrentWindowValuesUptoLast period (mkRideAssignedKey customerId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)
  return (cancelledCount, assignedCount)

data CancellationRateBasedNudgingAndBlockingConfig = CancellationRateBasedNudgingAndBlockingConfig
  { cancellationRateThresholdDaily :: Int,
    cancellationRateThresholdWeekly :: Int,
    cancellationRateThresholdMonthly :: Maybe Int,
    dailyMinRidesforBlocking :: Int,
    weeklyMinRidesforBlocking :: Int,
    monthlyMinRidesforBlocking :: Maybe Int,
    dailyMinRidesforNudging :: Int,
    weeklyMinRidesforNudging :: Int,
    monthlyMinRidesforNudging :: Maybe Int,
    dailyOffenceSuspensionTimeHours :: Int,
    weeklyOffenceSuspensionTimeHours :: Int,
    monthlyOffenceSuspensionTimeHours :: Maybe Int,
    dailyConditionCooldownTimeHours :: Maybe Int,
    weeklyConditionCooldownTimeHours :: Maybe Int,
    monthlyConditionCooldownTimeHours :: Maybe Int
  }
  deriving (Show, Eq, Generic)

nudgeOrBlockCustomer ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, JobCreator r m, HasShortDurationRetryCfg r c) =>
  RC.RiderConfig ->
  DP.Person ->
  m ()
nudgeOrBlockCustomer riderConfig customer = do
  unless (riderConfig.enableCustomerCancellationRateBlocking == Just True) $ do
    logDebug $ "Customer cancellation rate blocking is disabled for city: " <> customer.merchantOperatingCityId.getId
    return ()

  activeJourneys <- QJourney.findAllActiveByRiderId customer.id
  unless (null activeJourneys) $ do
    logDebug $ "Customer " <> customer.id.getId <> " has active journeys, skipping cancellation rate blocking check"
    return ()
  personFlowStatus <- QPFS.getStatus customer.id
  mbLatestBooking <- QBExtra.findLatestSelfAndPartyBookingByRiderId customer.id
  let hasActiveBooking = isJust mbLatestBooking
  let isNotOnRide = case personFlowStatus of
        Just DPFS.IDLE -> not hasActiveBooking
        Just (DPFS.FEEDBACK_PENDING _) -> not hasActiveBooking
        _ -> False

  unless isNotOnRide $ do
    logDebug $ "Customer " <> customer.id.getId <> " is on ride or has active booking, skipping cancellation rate blocking check"
    return ()
  let config = buildConfig riderConfig
  case (riderConfig.cancellationRateWindow, config) of
    (Just windowSize, Just CancellationRateBasedNudgingAndBlockingConfig {..}) -> do
      (dailyCancellationRate, dailyAssignedCount) <- getCancellationRateOfDays 1 windowSize
      (weeklyCancellationRate, weeklyAssignedCount) <- getCancellationRateOfDays 7 windowSize
      (monthlyCancellationRate, monthlyAssignedCount) <- getCancellationRateOfDays 30 windowSize

      logDebug $ "All cancellation rate data for customerId: " <> customer.id.getId <> ": dailyCancellationRate: " <> show dailyCancellationRate <> " dailyAssignedCount: " <> show dailyAssignedCount <> " weeklyCancellationRate: " <> show weeklyCancellationRate <> " weeklyAssignedCount: " <> show weeklyAssignedCount <> " monthlyCancellationRate: " <> show monthlyCancellationRate <> " monthlyAssignedCount: " <> show monthlyAssignedCount

      let canBlockOnWeekly = isCustomerBlockable cancellationRateThresholdWeekly weeklyMinRidesforBlocking weeklyCancellationRate weeklyAssignedCount
      let canBlockOnDaily = isCustomerBlockable cancellationRateThresholdDaily dailyMinRidesforBlocking dailyCancellationRate dailyAssignedCount
      -- Monthly blocking only if all monthly values are present (threshold, minRides, suspensionTime)
      let canBlockOnMonthly = case (cancellationRateThresholdMonthly, monthlyMinRidesforBlocking, monthlyOffenceSuspensionTimeHours) of
            (Just threshold, Just minRides, Just suspensionTime) ->
              isCustomerBlockable threshold minRides monthlyCancellationRate monthlyAssignedCount && suspensionTime > 0
            _ -> False

      case (canBlockOnWeekly, canBlockOnDaily, canBlockOnMonthly) of
        (True, _, _) -> do
          let suspensionTimeHours = weeklyOffenceSuspensionTimeHours
          blockCustomerTemporarily customer.merchantId customer.merchantOperatingCityId customer.id suspensionTimeHours
        (_, True, _) -> do
          let suspensionTimeHours = dailyOffenceSuspensionTimeHours
          blockCustomerTemporarily customer.merchantId customer.merchantOperatingCityId customer.id suspensionTimeHours
        (_, _, True) -> do
          let suspensionTimeHours = fromMaybe 0 monthlyOffenceSuspensionTimeHours
          when (suspensionTimeHours > 0) $ do
            blockCustomerTemporarily customer.merchantId customer.merchantOperatingCityId customer.id suspensionTimeHours
        _ -> do
          when (canNudgeCustomer cancellationRateThresholdWeekly weeklyMinRidesforNudging weeklyMinRidesforBlocking weeklyCancellationRate weeklyAssignedCount) $ do
            logDebug $ "Would nudge customer for weekly cancellation rate: " <> show weeklyCancellationRate
          when (canNudgeCustomer cancellationRateThresholdDaily dailyMinRidesforNudging dailyMinRidesforBlocking dailyCancellationRate dailyAssignedCount) $ do
            logDebug $ "Would nudge customer for daily cancellation rate: " <> show dailyCancellationRate
          when (maybe False (\threshold -> maybe False (\minRidesNudge -> maybe False (\minRidesBlock -> canNudgeCustomer threshold minRidesNudge minRidesBlock monthlyCancellationRate monthlyAssignedCount) monthlyMinRidesforBlocking) monthlyMinRidesforNudging) cancellationRateThresholdMonthly) $ do
            logDebug $ "Would nudge customer for monthly cancellation rate: " <> show monthlyCancellationRate
    _ -> logInfo "cancellationRateWindow or cancellationRateBasedNudgingAndBlockingConfig not found in rider config"
  where
    canNudgeCustomer cancellationRateThreshold minAssignedRides maxAssignedRides cancellationRate rideAssignedCount =
      (cancellationRate > cancellationRateThreshold) && (rideAssignedCount > minAssignedRides && rideAssignedCount <= maxAssignedRides)

    isCustomerBlockable cancellationRateThreshold rideAssignedThreshold cancellationRate assignedCount =
      (cancellationRate >= cancellationRateThreshold) && (assignedCount >= rideAssignedThreshold)

    getCancellationRateOfDays period windowSize = do
      let windowInt = toInteger windowSize
      cancelledCount <- fmap (sum . map (fromMaybe 0)) $ Redis.withCrossAppRedis $ SWC.getCurrentWindowValuesUptoLast period (mkRideCancelledKey customer.id.getId) (SWC.SlidingWindowOptions windowInt SWC.Days)
      assignedCount <- fmap (sum . map (fromMaybe 0)) $ Redis.withCrossAppRedis $ SWC.getCurrentWindowValuesUptoLast period (mkRideAssignedKey customer.id.getId) (SWC.SlidingWindowOptions windowInt SWC.Days)
      let cancellationRate = ((cancelledCount) * 100) `div` max 1 (assignedCount)
      return (cancellationRate, assignedCount)

    buildConfig :: RC.RiderConfig -> Maybe CancellationRateBasedNudgingAndBlockingConfig
    buildConfig config = do
      cancellationRateThresholdDaily <- config.cancellationRateThresholdDaily
      cancellationRateThresholdWeekly <- config.cancellationRateThresholdWeekly
      let cancellationRateThresholdMonthly = config.cancellationRateThresholdMonthly
      dailyMinRidesforBlocking <- config.dailyMinRidesForBlocking
      weeklyMinRidesforBlocking <- config.weeklyMinRidesForBlocking
      let monthlyMinRidesforBlocking = config.monthlyMinRidesForBlocking
      dailyMinRidesforNudging <- config.dailyMinRidesForNudging
      weeklyMinRidesforNudging <- config.weeklyMinRidesForNudging
      let monthlyMinRidesforNudging = config.monthlyMinRidesForNudging
      dailyOffenceSuspensionTimeHours <- config.dailyOffenceSuspensionTimeHours
      weeklyOffenceSuspensionTimeHours <- config.weeklyOffenceSuspensionTimeHours
      let monthlyOffenceSuspensionTimeHours = config.monthlyOffenceSuspensionTimeHours
      let dailyConditionCooldownTimeHours = config.dailyConditionCooldownTimeHours
      let weeklyConditionCooldownTimeHours = config.weeklyConditionCooldownTimeHours
      let monthlyConditionCooldownTimeHours = config.monthlyConditionCooldownTimeHours
      return CancellationRateBasedNudgingAndBlockingConfig {..}

blockCustomerTemporarily ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, JobCreator r m, HasShortDurationRetryCfg r c) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Int ->
  m ()
blockCustomerTemporarily merchantId merchantOperatingCityId customerId suspensionTimeHours = do
  now <- getCurrentTime
  let blockedUntil = addUTCTime (fromIntegral suspensionTimeHours * 60 * 60) now
  void $ QPExtra.updatingBlockedStateWithUntil customerId Nothing True (Just blockedUntil)
  let unblockCustomerJobTs = secondsToNominalDiffTime (fromIntegral suspensionTimeHours) * 60 * 60
  JC.createJobIn @_ @'RJS.UnblockCustomer (Just merchantId) (Just merchantOperatingCityId) unblockCustomerJobTs $
    RJS.UnblockCustomerJobData
      { customerId = customerId
      }
  logInfo $ "Temporarily blocking customer, customerId: " <> customerId.getId <> " until: " <> show blockedUntil <> ". Unblock job scheduled."
