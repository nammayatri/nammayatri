module SharedLogic.Scheduler.Jobs.DailyPassStatusUpdate where

import qualified Data.Time as Time
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import qualified Storage.Queries.PurchasedPassPayment as QPurchasedPassPayment

dailyPassStatusUpdate ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EncFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'DailyPassStatusUpdate ->
  m ExecutionResult
dailyPassStatusUpdate Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      merchantId' = jobData.merchantId
      merchantOpCityId = jobData.merchantOperatingCityId
      autoSchedule = jobData.autoSchedule

  mbRiderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOpCityId.getId})
  let timeDiffFromUtc = maybe (Seconds 19800) (.timeDiffFromUtc) mbRiderConfig
      batchSize = max 1 $ fromMaybe 1000 (mbRiderConfig >>= (.passStatusUpdateBatchSize))

  istTime <- getLocalCurrentTime timeDiffFromUtc
  let today = Time.utctDay istTime

  expiredCount <- expireBatch merchantOpCityId today batchSize
  let remainingBudget = batchSize - expiredCount
  activatedCount <-
    if remainingBudget > 0
      then activateBatch merchantOpCityId today remainingBudget
      else pure 0

  logInfo $
    "DailyPassStatusUpdate: expired="
      <> show expiredCount
      <> " activated="
      <> show activatedCount
      <> " budget="
      <> show batchSize

  when autoSchedule $
    if expiredCount + activatedCount < batchSize
      then scheduleTomorrow merchantId' merchantOpCityId timeDiffFromUtc istTime
      else createJobIn @_ @'DailyPassStatusUpdate (Just merchantId') (Just merchantOpCityId) 0 jobData

  pure Complete

expireBatch ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Time.Day ->
  Int ->
  m Int
expireBatch merchantOpCityId today batchSize = do
  passes <-
    QPurchasedPass.findAllByStatusesAndEndDateLessThan
      merchantOpCityId
      [DPurchasedPass.Active, DPurchasedPass.PreBooked]
      today
      (Just batchSize)
      (Just 0)
  let passIds = map (.id) passes
  QPurchasedPass.updateStatusByIds DPurchasedPass.Expired passIds
  QPurchasedPassPayment.expireOlderPaymentsByPurchasedPassIds passIds today
  pure (length passes)

activateBatch ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Time.Day ->
  Int ->
  m Int
activateBatch merchantOpCityId today budget = do
  passes <-
    QPurchasedPass.findAllPreBookedByStartDateRange
      merchantOpCityId
      today
      (Just budget)
      (Just 0)
  let passIds = map (.id) passes
  QPurchasedPass.updateStatusByIds DPurchasedPass.Active passIds
  QPurchasedPassPayment.activatePreBookedPaymentsByPurchasedPassIds passIds today
  pure (length passes)

scheduleTomorrow ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Seconds ->
  UTCTime ->
  m ()
scheduleTomorrow merchantId' merchantOpCityId timeDiffFromUtc istTime = do
  let tzMinutes = getSeconds timeDiffFromUtc `div` 60
      tz = Time.minutesToTimeZone tzMinutes
      tomorrow = Time.addDays 1 (Time.utctDay istTime)
      localTime0100 = Time.LocalTime tomorrow (Time.TimeOfDay 1 0 0)
      tomorrowAt0100UTC = Time.localTimeToUTC tz localTime0100
  now <- getCurrentTime
  let delay = diffUTCTime tomorrowAt0100UTC now
      -- Date-keyed guard: any same-day re-trigger (cron + dashboard, or repeated dashboard
      -- presses) targets the same lock and is dropped, preventing the tomorrow-01:00 queue
      -- from accumulating duplicates.
      lockKey = "DailyPassStatusUpdate:nextRun:" <> merchantOpCityId.getId <> ":" <> show tomorrow
      lockTtlSeconds = max 60 (round delay + 1800)
      nextJobData =
        DailyPassStatusUpdateJobData
          { merchantId = merchantId',
            merchantOperatingCityId = merchantOpCityId,
            autoSchedule = True
          }
  claimed <- Hedis.tryLockRedis lockKey lockTtlSeconds
  if claimed
    then do
      createJobIn @_ @'DailyPassStatusUpdate (Just merchantId') (Just merchantOpCityId) delay nextJobData
      logInfo "DailyPassStatusUpdate: drained, scheduled next run at tomorrow 01:00 local"
    else logInfo "DailyPassStatusUpdate: drained, next-day run already enqueued — skipping reschedule"
