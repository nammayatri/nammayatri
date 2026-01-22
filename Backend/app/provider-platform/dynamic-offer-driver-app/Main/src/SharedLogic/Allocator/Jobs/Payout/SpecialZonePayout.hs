module SharedLogic.Allocator.Jobs.Payout.SpecialZonePayout where

import qualified Domain.Types.ScheduledPayout as DSP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator
import qualified Storage.Queries.ScheduledPayout as QSP
import qualified Storage.Queries.ScheduledPayoutExtra as QSPE

sendSpecialZonePayout ::
  ( EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Job 'SpecialZonePayout ->
  m ExecutionResult
sendSpecialZonePayout Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let SpecialZonePayoutJobData {scheduledPayoutId} = jobInfo.jobData
  let lockKey = "payout:lock:" <> scheduledPayoutId.getId

  -- 1. Try to acquire Redis lock (5 min TTL)
  acquired <- Redis.tryLockRedis lockKey 300
  if not acquired
    then do
      logWarning $ "Could not acquire lock for payout: " <> show scheduledPayoutId
      pure Complete
    else do
      -- Process with lock, release on exit
      flip finally (Redis.unlockRedis lockKey) $ do
        -- 2. Fetch ScheduledPayout record
        mbScheduledPayout <- QSP.findById scheduledPayoutId

        case mbScheduledPayout of
          Nothing -> do
            logInfo $ "ScheduledPayout record not found for id: " <> show scheduledPayoutId
            pure Complete
          Just scheduledPayout -> do
            -- 3. Check status (idempotency + cancellation check)
            case scheduledPayout.status of
              DSP.FAILED -> do
                logInfo $ "Payout was cancelled/failed, skipping: " <> show scheduledPayoutId
                pure Complete
              DSP.CREDITED -> do
                logInfo $ "Payout already processed: " <> show scheduledPayoutId
                pure Complete
              DSP.PROCESSING -> do
                logInfo $ "Payout already being processed: " <> show scheduledPayoutId
                pure Complete
              DSP.AUTO_PAY_FAILED -> do
                logInfo $ "Payout auto-pay failed, needs admin retry: " <> show scheduledPayoutId
                pure Complete
              DSP.RETRYING -> do
                logInfo $ "Payout is being retried: " <> show scheduledPayoutId
                pure Complete
              DSP.INITIATED -> do
                -- 4. Mark as PROCESSING with history
                QSPE.updateStatusWithHistoryById DSP.PROCESSING (Just "Payment in progress") scheduledPayout

                -- 5. Execute payout logic (Placeholder)
                -- TODO: Call actual payout service here
                logInfo $ "Processing Special Zone Payout for ride: " <> scheduledPayout.rideId

                -- 6. Update status to CREDITED with history (on success)
                -- TODO: Handle failure case and mark as AUTO_PAY_FAILED
                QSPE.updateStatusWithHistoryById DSP.CREDITED (Just "Payment credited to bank") scheduledPayout

                logInfo $ "Special Zone Payout processed successfully for id: " <> show scheduledPayoutId
                pure Complete
