{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.BehaviourManagement.CustomerCancellationRate where

import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.RiderConfig as RC
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import Lib.ConfigPilot.Interface.Types (getConfig)
import Lib.Scheduler.Environment
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import qualified SharedLogic.JobScheduler as RJS
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.BookingExtra as QBExtra
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.PersonExtra as QPExtra
import Tools.Error
import qualified Tools.Notifications as Notify

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

mkRiderStatsKey :: Text -> Time.Day -> Text
mkRiderStatsKey riderId day =
  "customer-stats:" <> riderId <> ":" <> T.pack (Time.showGregorian day)

mkRiderStatsFieldKey :: Text -> Time.Day -> RiderStatEvent -> Text
mkRiderStatsFieldKey riderId day event =
  mkRiderStatsKey riderId day <> ":" <> eventField event

retentionDays :: Int
retentionDays = 90

data RiderStatEvent = Assigned | Completed | Cancelled
  deriving (Eq, Show)

eventField :: RiderStatEvent -> Text
eventField Assigned = "assigned"
eventField Completed = "completed"
eventField Cancelled = "cancelled"

readRiderStatCount ::
  (Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Text ->
  Time.Day ->
  RiderStatEvent ->
  m Integer
readRiderStatCount riderId day event = do
  let key = mkRiderStatsKey riderId day
      fieldKey = mkRiderStatsFieldKey riderId day event
      field = eventField event
  mbAtomicVal <- Redis.get @Text fieldKey
  case mbAtomicVal of
    Just val -> pure $ parseTextCount (Just val)
    Nothing -> Redis.hGet @Text key field <&> parseTextCount
  where
    parseTextCount = maybe 0 (fromMaybe 0 . readMaybe . T.unpack)

readLegacySlidingWindowCount ::
  (Redis.HedisFlow m r, MonadFlow m) =>
  Text ->
  RiderStatEvent ->
  Integer ->
  m Integer
readLegacySlidingWindowCount riderId event windowDays = do
  let opts = SWC.SlidingWindowOptions windowDays SWC.Days
  case event of
    Assigned -> SWC.getCurrentWindowCount (mkRideAssignedKey riderId) opts
    Cancelled -> SWC.getCurrentWindowCount (mkRideCancelledKey riderId) opts
    Completed -> pure 0

incrementRiderStatToday ::
  (Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r, MonadFlow m) =>
  Id DP.Person ->
  RiderStatEvent ->
  Integer ->
  m ()
incrementRiderStatToday personId event count = Redis.withCrossAppRedis $ do
  today <- Time.utctDay <$> getCurrentTime
  let key = mkRiderStatsFieldKey personId.getId today event
      ttl = retentionDays * 86400
  newCount <- Redis.incrby key count
  when (newCount == count) $
    Redis.expire key ttl

sumRiderStatOverWindow ::
  (Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r, MonadFlow m) =>
  Id DP.Person ->
  RiderStatEvent ->
  Integer ->
  m Integer
sumRiderStatOverWindow personId event windowDays = Redis.withCrossAppRedis $ do
  today <- Time.utctDay <$> getCurrentTime
  let keys =
        [ Time.addDays (- i) today
          | i <- [0 .. windowDays - 1]
        ]
  results <- forM keys $ \day -> readRiderStatCount personId.getId day event
  let total = sum results
  if total > 0 || event == Completed
    then pure total
    else readLegacySlidingWindowCount personId.getId event windowDays

getRiderStatPerDayOverWindow ::
  (Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r, MonadFlow m) =>
  Id DP.Person ->
  RiderStatEvent ->
  Integer ->
  m [Integer]
getRiderStatPerDayOverWindow personId event windowDays = Redis.withCrossAppRedis $ do
  today <- Time.utctDay <$> getCurrentTime
  forM [0 .. windowDays - 1] $ \i -> do
    let day = Time.addDays (- i) today
    readRiderStatCount personId.getId day event

getWindowSize :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m Integer
getWindowSize mocId = do
  riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = mocId.getId}) (Just (CQRC.findByMerchantOperatingCityId mocId)) >>= fromMaybeM (RiderConfigDoesNotExist mocId.getId)
  pure $ toInteger $ fromMaybe 30 riderConfig.cancellationRateWindow

incrementCancelledCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  Id DP.Person ->
  Integer ->
  m ()
incrementCancelledCount customerId _windowSize =
  incrementRiderStatToday customerId Cancelled 1

incrementAssignedCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  Id DP.Person ->
  Integer ->
  m ()
incrementAssignedCount customerId _windowSize =
  incrementRiderStatToday customerId Assigned 1

incrementCompletedCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  Id DP.Person ->
  Integer ->
  m ()
incrementCompletedCount customerId count =
  incrementRiderStatToday customerId Completed count

getCancellationCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  Integer ->
  Id DP.Person ->
  m Integer
getCancellationCount windowSize customerId = sumRiderStatOverWindow customerId Cancelled windowSize

getAssignedCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  Integer ->
  Id DP.Person ->
  m Integer
getAssignedCount windowSize customerId = sumRiderStatOverWindow customerId Assigned windowSize

getCompletedCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  Integer ->
  Id DP.Person ->
  m Integer
getCompletedCount windowSize customerId = sumRiderStatOverWindow customerId Completed windowSize

getCancellationRateData ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m (Maybe CustomerCancellationRateData)
getCancellationRateData mocId customerId = do
  riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = mocId.getId}) (Just (CQRC.findByMerchantOperatingCityId mocId)) >>= fromMaybeM (RiderConfigDoesNotExist mocId.getId)
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
getCancellationRateOfDaysStandalone customerId period _windowSize = do
  cancelledCounts <- getRiderStatPerDayOverWindow customerId Cancelled period
  assignedCounts <- getRiderStatPerDayOverWindow customerId Assigned period
  return (sum cancelledCounts, sum assignedCounts)

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
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, JobCreator r m, HasShortDurationRetryCfg r c, ServiceFlow m r) =>
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
            Notify.sendCustomerCancellationRateNudge customer "CUSTOMER_CANCELLATION_RATE_NUDGE_WEEKLY" weeklyCancellationRate
          when (canNudgeCustomer cancellationRateThresholdDaily dailyMinRidesforNudging dailyMinRidesforBlocking dailyCancellationRate dailyAssignedCount) $ do
            Notify.sendCustomerCancellationRateNudge customer "CUSTOMER_CANCELLATION_RATE_NUDGE_DAILY" dailyCancellationRate
          when (maybe False (\threshold -> maybe False (\minRidesNudge -> maybe False (\minRidesBlock -> canNudgeCustomer threshold minRidesNudge minRidesBlock monthlyCancellationRate monthlyAssignedCount) monthlyMinRidesforBlocking) monthlyMinRidesforNudging) cancellationRateThresholdMonthly) $ do
            Notify.sendCustomerCancellationRateNudge customer "CUSTOMER_CANCELLATION_RATE_NUDGE_MONTHLY" monthlyCancellationRate
    _ -> logInfo "cancellationRateWindow or cancellationRateBasedNudgingAndBlockingConfig not found in rider config"
  where
    canNudgeCustomer cancellationRateThreshold minAssignedRides maxAssignedRides cancellationRate rideAssignedCount =
      (cancellationRate > cancellationRateThreshold) && (rideAssignedCount > minAssignedRides && rideAssignedCount <= maxAssignedRides)

    isCustomerBlockable cancellationRateThreshold rideAssignedThreshold cancellationRate assignedCount =
      (cancellationRate >= cancellationRateThreshold) && (assignedCount >= rideAssignedThreshold)

    getCancellationRateOfDays period _windowSize = do
      cancelledCounts <- getRiderStatPerDayOverWindow customer.id Cancelled period
      assignedCounts <- getRiderStatPerDayOverWindow customer.id Assigned period
      let cancelledCount = sum cancelledCounts
          assignedCount = sum assignedCounts
          cancellationRate = (cancelledCount * 100) `div` max 1 assignedCount
      return (fromIntegral cancellationRate, fromIntegral assignedCount)

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
