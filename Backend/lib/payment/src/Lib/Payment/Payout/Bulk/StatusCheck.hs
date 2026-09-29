-- NOTE (reviewer, remove before merge): NEW module, not on main (bulk lifecycle: "status check"). The body
--   of the per-city BulkPayoutStatusCheck allocator job. HDFC has no webhook and no per-order status API,
--   so this job is the only way we learn a bulk payout's outcome.
--   Caller chain: allocator job BulkPayoutStatusCheck (SharedLogic/Allocator/Jobs/Payout/
--   BulkPayoutStatusCheck.hs) -> runCityStatusCheckJob driverBulkHandle <jobData.merchantOperatingCityId>
--   -> checkCityBatches -> stepOneBatch -> Bulk.Resolve.advanceBatch. The job is created only by
--   Bulk.Schedule.ensureCityJob, which runs only when a bulk batch is opened.
--   Juspay/Stripe impact: bulk-only. It reads only payout_batch rows, and only the bulk flow writes them.
--   Juspay/Stripe keep main's per-order status job (Lib.Payment.Payout.StatusCheck), webhook and order
--   refresh.

-- | One job per city that asks HDFC about that city's bulk payout batches in flight. It is created
-- when the city's first batch is (see 'Lib.Payment.Payout.Bulk.Schedule.ensureCityJob') and then
-- keeps rescheduling itself.
--
-- Each batch carries the time of its next call (@payout_batch.nextStatusCallAt@). A run takes
-- whatever is due and makes one call per batch. If it found anything due it runs again 5 seconds
-- later; if it found nothing it waits the city's interval. A call that comes due between runs is
-- simply made a little late, which HDFC does not mind -- a later status call still gets the full
-- answer. A check whose time passed while the job was down is due like any other, so an outage
-- needs no recovery path.
module Lib.Payment.Payout.Bulk.StatusCheck
  ( runCityStatusCheckJob,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import Lib.Payment.Payout.Bulk.Resolve (advanceBatch)
import qualified Lib.Payment.Payout.Bulk.Schedule as BSC
import Lib.Payment.Payout.Bulk.Types
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import qualified Lib.Payment.Storage.Queries.PayoutBatchExtra as QPayoutBatchExtra
import Lib.Scheduler.Types (ExecutionResult (..))

-- | When the next run starts after a run that found batches due: soon, to work through a backlog.
--   That is also why a run's batch limit can be small: whatever it leaves is still due, and the
--   next run takes it.
busyRerunDelay :: NominalDiffTime
busyRerunDelay = 5

-- | No new call is started after this long. The scheduler holds a per-job lock while a run
--   executes, so a run has to end well inside it.
runBudget :: NominalDiffTime
runBudget = 30

-- NOTE (reviewer, remove before merge): one job per city, checking only that city's batches (the city comes
--   from the job data {merchantId, merchantOperatingCityId}). It always returns ReSchedule, so a city's job
--   never ends by itself: 5 s later when the run found batches due, else after the city's interval
--   (ScheduledPayoutConfig.bulkStatusCheckIntervalMinutes, default 5 min). A call made up to one interval
--   late still gets the full answer from HDFC. Bulk-only.

-- | One run of one city's status-check job: that city's due batches only. The app's job handler
--   calls this with its 'Handle', the city in the job's data, and the city's idle interval and batch
--   limit. An error never ends the job: it is logged and the job runs again after the interval.
runCityStatusCheckJob ::
  (BulkFlow m r) =>
  Handle m ->
  Text -> -- merchantOperatingCityId
  NominalDiffTime -> -- how long to wait after a run that found nothing due
  Int -> -- most batches one run takes
  m ExecutionResult
runCityStatusCheckJob h cityId idleInterval batchLimit = do
  result <- try (checkCityBatches h cityId batchLimit)
  now <- getCurrentTime
  delay <- case result of
    Right foundDue -> pure (if foundDue then busyRerunDelay else idleInterval)
    Left (err :: SomeException) -> do
      logError $ "Bulk payout status check failed for city " <> cityId <> "; trying again at the next run: " <> show err
      pure idleInterval
  pure (ReSchedule (addUTCTime delay now))

-- NOTE (reviewer, remove before merge): one list query per run (Storage/Queries/PayoutBatchExtra.findDueForStatusHit;
--   index idx_payout_batch_moc_next_status_call_at, migration 0893). At most the city's batch limit
--   (ScheduledPayoutConfig.bulkStatusCheckBatchLimit, default 50) and 30 s per run; anything left over is still
--   due, so the next run (5 s later) takes it. Bulk-only.

-- | Check the city's due batches. True when there were any, so the job comes back soon.
checkCityBatches ::
  (BulkFlow m r) =>
  Handle m ->
  Text ->
  Int ->
  m Bool
checkCityBatches h cityId batchLimit = do
  start <- getCurrentTime
  due <- QPayoutBatchExtra.findDueForStatusHit cityId start (max 1 batchLimit)
  forM_ due $ \batch -> do
    now <- getCurrentTime
    -- Out of budget: the batch is still due, so the next run takes it.
    unless (diffUTCTime now start > runBudget) $ stepOneBatch h batch
  pure (not (null due))

-- NOTE (reviewer, remove before merge): the batch lock (Bulk.Schedule.batchLockKey, 120 s, no waiting) plus
--   a fresh re-read of the row: if two copies of the job run at once, only one of them calls HDFC
--   for a batch and the other skips it. Bulk-only.

-- | One batch: take its lock, read it again, and call HDFC once if it is still due.
--
--   The re-read is what makes a duplicated run harmless -- the list came from Postgres, which
--   lags the write path, so a batch another run has already moved on can still appear here.
--   Another run holding the lock owns the batch for now; this run leaves it alone.
stepOneBatch ::
  (BulkFlow m r) =>
  Handle m ->
  DPayoutBatch.PayoutBatch ->
  m ()
stepOneBatch h batch =
  void . Redis.whenWithLockRedisAndReturnValue (BSC.batchLockKey h batch.id) 120 $ do
    now <- getCurrentTime
    latestBatch <- QPayoutBatch.findByPrimaryKey batch.id
    case latestBatch of
      Nothing -> logError $ "Status check: batch vanished between listing and locking: " <> batch.id.getId
      Just fresh -> when (maybe False (<= now) fresh.nextStatusCallAt) $ void (advanceBatch h fresh)
