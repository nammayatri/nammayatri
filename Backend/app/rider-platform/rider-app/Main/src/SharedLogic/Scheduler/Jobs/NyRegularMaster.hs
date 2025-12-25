{-# LANGUAGE FlexibleContexts #-}

module SharedLogic.Scheduler.Jobs.NyRegularMaster where

import qualified Data.Time as Time
import qualified Domain.Types.NyRegularSubscription as NyRegularSubscription
import Kernel.External.Encryption (EncFlow)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime, secondsToNominalDiffTime)
import Kernel.Utils.Logging (logInfo)
import Lib.Scheduler (ExecutionResult (..), Job (..))
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import SharedLogic.NyRegularSubscriptionHasher (calculateSubscriptionSchedulingHash)
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as QCMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as RiderConfig
import qualified Storage.Queries.NyRegularSubscription as NyRegularSubscription
import qualified Tools.Error as Tools

runNyRegularMasterJob ::
  ( MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  Job 'NyRegularMaster ->
  m ExecutionResult
runNyRegularMasterJob Job {merchantOperatingCityId} = do
  cityId <- merchantOperatingCityId & fromMaybeM (Tools.InternalError "Job is missing merchantOperatingCityId")
  merchantCity <- QCMOC.findById cityId >>= fromMaybeM (Tools.MerchantOperatingCityNotFound $ "merchantOpCityid: " <> cityId.getId)
  riderConfig <- RiderConfig.findByMerchantOperatingCityId cityId Nothing >>= fromMaybeM (Tools.RiderConfigNotFound $ "merchantOpCityid: " <> cityId.getId)
  let batchSize = fromMaybe 10 (riderConfig.nyRegularSubscriptionBatchSize)
      executionTimeOffsetMinutes = fromMaybe 15 (riderConfig.nyRegularExecutionTimeOffsetMinutes)

  now <- getCurrentTime
  let timeDiffFromUtc = secondsToNominalDiffTime (riderConfig.timeDiffFromUtc)
  let localNow = Time.addUTCTime timeDiffFromUtc now
  let today = Time.utctDay now
  subscriptions <- NyRegularSubscription.findAllActiveAt cityId today (Just batchSize)
  logInfo $ "Found " <> show (length subscriptions) <> " active subscriptions to process for city " <> cityId.getId
  traverse_ (processSubscription now localNow timeDiffFromUtc executionTimeOffsetMinutes) subscriptions

  -- If we processed a full batch, there might be more for this city.
  -- Reschedule the same job to run again after a delay.
  if length subscriptions == batchSize
    then do
      rescheduleTime <- Time.addUTCTime 10 <$> getCurrentTime
      logInfo $ "Rescheduling master job to process next batch for city " <> cityId.getId
      return $ ReSchedule rescheduleTime
    else do
      let offsetSeconds = riderConfig.nyRegularMasterJobNextRunOffsetSeconds
          nextDay = Time.addDays 1 (Time.utctDay localNow)
          -- Local midnight in UTC
          localMidnightNextDayLocal = Time.UTCTime nextDay 0
          nextRunTimeLocal = Time.addUTCTime (offsetSeconds) localMidnightNextDayLocal
          nextRunTimeUTC = Time.addUTCTime (-1 * secondsToNominalDiffTime riderConfig.timeDiffFromUtc) nextRunTimeLocal
          scheduleAfter = Time.diffUTCTime nextRunTimeUTC now
      void $ createJobIn @_ @'NyRegularMaster (Just merchantCity.merchantId) (Just merchantCity.id) scheduleAfter ()
      logInfo $ "Scheduling next NYRegular master job for city " <> cityId.getId <> " at local time: " <> show nextRunTimeLocal <> " (UTC: " <> show nextRunTimeUTC <> ")"
      return Complete

processSubscription ::
  ( MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  Time.UTCTime ->
  Time.UTCTime ->
  Time.NominalDiffTime ->
  Int ->
  NyRegularSubscription.NyRegularSubscription ->
  m ()
processSubscription now localNow utcOffset executionTimeOffsetMinutes subscription = do
  let today = Time.utctDay now
  let todayLocal = Time.utctDay localNow
  when (isValid today todayLocal subscription) $ do
    logInfo $ "Processing subscription: " <> show subscription.id
    createNyRegularInstanceJob executionTimeOffsetMinutes utcOffset subscription
    -- Mark as processed for today
    void $ NyRegularSubscription.updateLastProcessedAtById (Just localNow) subscription.id
  where
    isValid today' todayLocal sub =
      and
        [ sub.status == NyRegularSubscription.ACTIVE,
          maybe True (> today') sub.recurrenceEndDate,
          not (isDateInPauseRange today' (sub.pauseStartDate, sub.pauseEndDate)),
          toDayOfWeek todayLocal `elem` sub.recurrenceRuleDays
        ]

    isDateInPauseRange day (Just start, Just end) =
      let startDay = Time.utctDay start
          endDay = Time.utctDay end
       in day >= startDay && day <= endDay
    isDateInPauseRange _ _ = False

    toDayOfWeek = Time.dayOfWeek

createNyRegularInstanceJob ::
  ( MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  Int ->
  Time.NominalDiffTime ->
  NyRegularSubscription.NyRegularSubscription ->
  m ()
createNyRegularInstanceJob executionTimeOffsetMinutes utcOffset subscription = do
  now <- getCurrentTime
  let localNow = Time.addUTCTime utcOffset now
      today = Time.utctDay localNow
  -- Get rider config to access timeDiffFromUtc

  -- Calculate scheduled time using timeDiffFromUtc instead of localTimeToUTC
  let scheduledTimeLocal = Time.UTCTime today (Time.timeOfDayToTime subscription.scheduledTimeOfDay)
      scheduledTimeUTC = Time.addUTCTime (-1 * utcOffset) scheduledTimeLocal

  -- Schedule the job X minutes before the pickup time (configurable)
  let executionTime = Time.addUTCTime (fromIntegral (- executionTimeOffsetMinutes) * 60) scheduledTimeUTC
  when (executionTime > now) $ do
    let scheduleAfter = Time.diffUTCTime executionTime now
    currentHash <- calculateSubscriptionSchedulingHash subscription
    let jobData =
          NyRegularInstanceJobData
            { nyRegularSubscriptionId = subscription.id,
              userId = subscription.userId,
              scheduledTime = scheduledTimeLocal,
              expectedSchedulingHash = show currentHash
            }
    void $ createJobIn @_ @'NyRegularInstance subscription.merchantId subscription.merchantOperatingCityId scheduleAfter (jobData :: NyRegularInstanceJobData)
