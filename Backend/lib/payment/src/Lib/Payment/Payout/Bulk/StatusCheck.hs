-- | Per-city job that polls HDFC for in-flight bulk batches. Each batch stores its next call time,
-- so a call missed while the job was down is simply due on the next run.
module Lib.Payment.Payout.Bulk.StatusCheck
  ( runCityStatusCheckJob,
  )
where

import qualified Data.Text as T
import qualified Kernel.External.Payout.Interface as Payout
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

-- | Rerun delay after a run that found work, so a backlog drains quickly even with a small batch limit.
busyRerunDelay :: NominalDiffTime
busyRerunDelay = 5

-- | No new call starts after this long, so a run ends well inside the scheduler's 1800 s job lock.
--   Checked before each batch, so one slow call (60 s timeout) can overrun it.
runBudget :: NominalDiffTime
runBudget = 120

-- | One run of a city's status-check job. An error never ends the job; it is logged and the job
--   runs again after the idle interval.
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
    Right moved -> pure (if moved then busyRerunDelay else idleInterval)
    Left (err :: SomeException) -> do
      logError $ "Bulk payout status check failed for city " <> cityId <> "; trying again at the next run: " <> show err
      pure idleInterval
  pure (ReSchedule (addUTCTime delay now))

-- | Check the city's due batches. True when something moved or was left for lack of time, so the
--   job comes back soon.
checkCityBatches ::
  (BulkFlow m r) =>
  Handle m ->
  Text ->
  Int ->
  m Bool
checkCityBatches h cityId batchLimit = do
  start <- getCurrentTime
  due <- QPayoutBatchExtra.findDueForStatusHit cityId start (max 1 batchLimit)
  moved <- forM due $ \batch -> do
    now <- getCurrentTime
    -- Out of budget: the batch is still due, so the next run takes it.
    if diffUTCTime now start > runBudget
      then pure True
      else do
        res <- try (stepOneBatch h batch)
        case res of
          -- Only a real HDFC call resets the error count; a skipped batch proves nothing.
          Right calledHdfc -> do
            when calledHdfc $ clearBatchErrors h batch
            pure calledHdfc
          -- One batch's error must not hold up the rest of the city.
          Left (e :: SomeException) -> do
            logError $ "Status check failed for batch " <> batch.id.getId <> "; the rest of the city goes on: " <> show e
            countBatchError h batch e
            pure False
  pure (or moved)

-- | Errors in a row before a batch goes to manual review.
maxConsecutiveErrors :: Integer
maxConsecutiveErrors = 5

batchErrorsKey :: Handle m -> DPayoutBatch.PayoutBatch -> Text
batchErrorsKey h batch = h.keyPrefix <> "BulkPayoutStatusCheck:errors:" <> batch.id.getId

-- | Reset the error count after a clean call. Best effort: a Redis error only leaves the count as is.
clearBatchErrors :: (BulkFlow m r) => Handle m -> DPayoutBatch.PayoutBatch -> m ()
clearBatchErrors h batch = do
  res <- try (Redis.del (batchErrorsKey h batch))
  case res of
    Right () -> pure ()
    Left (e :: SomeException) -> logWarning $ "Could not reset the status-check error count for batch " <> batch.id.getId <> ": " <> show e

-- | Count one more error; at 'maxConsecutiveErrors' the batch goes to manual review, else it is
--   retried later. Best effort: if Redis or the DB is failing, it logs and leaves the batch due.
countBatchError :: (BulkFlow m r) => Handle m -> DPayoutBatch.PayoutBatch -> SomeException -> m ()
countBatchError h batch err = do
  res <- try $ do
    let key = batchErrorsKey h batch
    errors <- Redis.incr key
    Redis.expire key 86400
    if errors >= maxConsecutiveErrors
      then do
        let reason = "Status check failed " <> show errors <> " times in a row; last error: " <> T.take 300 (show err)
        logError $ "Payout batch " <> batch.id.getId <> " to manual review: " <> reason
        -- Clears nextStatusCallAt, so the batch is not polled again.
        QPayoutBatch.updateFailure DPayoutBatch.MANUAL_REVIEW_REQUIRED (Just reason) (Just "STATUS_CHECK_ERROR") Nothing Nothing batch.id
        Redis.del key
      else retryAfterError h batch
  case res of
    Right () -> pure ()
    Left (e :: SomeException) -> logError $ "Could not count the status-check error for batch " <> batch.id.getId <> ": " <> show e

-- | Push the next call out by the plan's errorRetryDelaySeconds instead of 5 s, without using up a
--   planned call. Only while still due and under the lock, so a fresh schedule is never overwritten.
retryAfterError :: (BulkFlow m r) => Handle m -> DPayoutBatch.PayoutBatch -> m ()
retryAfterError h batch = do
  mbPartner <- try (h.partnerFor batch)
  let plan = case mbPartner of
        Right (Right partner) -> Payout.bulkStatusCheckPlanOf partner
        Right (Left _) -> Payout.defaultBulkStatusCheckPlan
        Left (_ :: SomeException) -> Payout.defaultBulkStatusCheckPlan
  void . Redis.whenWithLockRedisAndReturnValue (BSC.batchLockKey h batch.id) 120 $ do
    now <- getCurrentTime
    fresh <- QPayoutBatch.findByPrimaryKey batch.id
    whenJust fresh $ \b ->
      when (maybe False (<= now) b.nextStatusCallAt) $
        QPayoutBatchExtra.updateNextStatusCallAt (addUTCTime (fromIntegral plan.errorRetryDelaySeconds) now) batch.id

-- | Lock the batch, re-read it, and call HDFC if still due; True if it called. The re-read matters
--   because the due list comes from Postgres, which can lag behind another run's write.
stepOneBatch ::
  (BulkFlow m r) =>
  Handle m ->
  DPayoutBatch.PayoutBatch ->
  m Bool
stepOneBatch h batch = do
  res <- Redis.whenWithLockRedisAndReturnValue (BSC.batchLockKey h batch.id) 120 $ do
    now <- getCurrentTime
    latestBatch <- QPayoutBatch.findByPrimaryKey batch.id
    case latestBatch of
      Nothing -> do
        logError $ "Status check: batch vanished between listing and locking: " <> batch.id.getId
        pure False
      Just fresh
        | maybe False (<= now) fresh.nextStatusCallAt -> advanceBatch h fresh >> pure True
        | otherwise -> pure False
  pure (either (const False) identity res)
