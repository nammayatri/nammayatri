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
  riderConfig <- RiderConfig.findByMerchantOperatingCityId cityId Nothing
  let batchSize = fromMaybe 10 (riderConfig >>= (.nyRegularSubscriptionBatchSize))
      executionTimeOffsetMinutes = fromMaybe 15 (riderConfig >>= (.nyRegularExecutionTimeOffsetMinutes))

  now <- getCurrentTime
  let today = Time.utctDay now
  subscriptions <- NyRegularSubscription.findAllActiveAt cityId today (Just batchSize)
  logInfo $ "Found " <> show (length subscriptions) <> " active subscriptions to process for city " <> cityId.getId
  traverse_ (processSubscription now executionTimeOffsetMinutes) subscriptions

  -- If we processed a full batch, there might be more for this city.
  -- Reschedule the same job to run again after a delay.
  if length subscriptions == batchSize
    then do
      rescheduleTime <- Time.addUTCTime 60 <$> getCurrentTime
      logInfo $ "Rescheduling master job to process next batch for city " <> cityId.getId
      return $ ReSchedule rescheduleTime
    else return Complete

processSubscription ::
  ( MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  Time.UTCTime ->
  Int ->
  NyRegularSubscription.NyRegularSubscription ->
  m ()
processSubscription now executionTimeOffsetMinutes subscription = do
  let today = Time.utctDay now
  when (isValid today subscription) $ do
    logInfo $ "Processing subscription: " <> show subscription.id
    createNyRegularInstanceJob executionTimeOffsetMinutes subscription
    -- Mark as processed for today
    void $ NyRegularSubscription.updateLastProcessedAtById (Just now) subscription.id
  where
    isValid today' sub =
      and
        [ sub.status == NyRegularSubscription.ACTIVE,
          maybe True (> today') sub.recurrenceEndDate,
          not (isDateInPauseRange today' (sub.pauseStartDate, sub.pauseEndDate)),
          toDayOfWeek today' `elem` sub.recurrenceRuleDays
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
  NyRegularSubscription.NyRegularSubscription ->
  m ()
createNyRegularInstanceJob executionTimeOffsetMinutes subscription = do
  now <- getCurrentTime
  let today = Time.utctDay now

  -- Get rider config to access timeDiffFromUtc
  riderConfig <- case subscription.merchantOperatingCityId of
    Nothing -> throwM $ Tools.InternalError "Subscription is missing merchantOperatingCityId, cannot determine local time."
    Just opCityId ->
      RiderConfig.findByMerchantOperatingCityId opCityId Nothing
        >>= fromMaybeM (Tools.InternalError $ "RiderConfig not found for merchantOperatingCityId: " <> opCityId.getId)

  let timeDiffFromUtc = riderConfig.timeDiffFromUtc
      -- Calculate scheduled time using timeDiffFromUtc instead of localTimeToUTC
      naiveUTCTime = Time.UTCTime today (Time.timeOfDayToTime subscription.scheduledTimeOfDay)
      scheduledTime = Time.addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) naiveUTCTime

  -- Schedule the job X minutes before the pickup time (configurable)
  let executionTime = Time.addUTCTime (fromIntegral (- executionTimeOffsetMinutes) * 60) scheduledTime
  when (executionTime > now) $ do
    let scheduleAfter = Time.diffUTCTime executionTime now
    currentHash <- calculateSubscriptionSchedulingHash subscription
    let jobData =
          NyRegularInstanceJobData
            { nyRegularSubscriptionId = subscription.id,
              userId = subscription.userId,
              scheduledTime = scheduledTime,
              expectedSchedulingHash = currentHash
            }
    void $ createJobIn @_ @'NyRegularInstance subscription.merchantId subscription.merchantOperatingCityId scheduleAfter (jobData :: NyRegularInstanceJobData)
