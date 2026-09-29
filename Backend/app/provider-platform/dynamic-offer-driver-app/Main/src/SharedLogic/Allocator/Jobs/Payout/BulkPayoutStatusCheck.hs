{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The one always-on job that asks HDFC about every bulk payout batch in flight.
--
-- It owns no schedule of its own: each batch carries the time of its next call
-- (@payout_batch.nextStatusCallAt@), and a run simply takes whatever is due, makes one call per
-- batch, and then sleeps until the earliest next one. A check whose time passed while the job was
-- down is due like any other, so an outage needs no recovery path.
--
-- Design: @Backend/dev/docs/hdfc-cbx-bulk-status-check-design.md@.
module SharedLogic.Allocator.Jobs.Payout.BulkPayoutStatusCheck (runBulkPayoutStatusCheck) where

import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import qualified Lib.Payment.Storage.Queries.PayoutBatchExtra as QPayoutBatchExtra
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.Payout.Bulk.Resolve (advanceBatch)
import qualified SharedLogic.Payout.BulkStatusCheck as BSC
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()

-- | Batches taken per run. A leftover page is not a problem: the run reschedules itself for two
--   seconds later and picks up where it stopped.
pageSize :: Int
pageSize = 50

-- | No new call is started after this long. The scheduler holds a per-job lock while a run
--   executes, so a run has to end well inside it.
runBudget :: NominalDiffTime
runBudget = 30

runBulkPayoutStatusCheck ::
  ( ServiceFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    HasKafkaProducer r,
    Redis.HedisLTSFlowEnv r,
    JobCreator r m
  ) =>
  Job 'BulkPayoutStatusCheck ->
  m ExecutionResult
runBulkPayoutStatusCheck Job {id} = withLogTag ("JobId-" <> id.getId) do
  BSC.refreshStatusCheckJobAlive
  start <- getCurrentTime
  due <- QPayoutBatchExtra.findDueForStatusHit start pageSize
  written <- forM due $ \batch -> do
    now <- getCurrentTime
    if diffUTCTime now start > runBudget
      then pure Nothing -- out of budget: this batch is still due, so the next run takes it
      else stepOneBatch batch
  nextAt <- QPayoutBatchExtra.findEarliestNextStatusHitAt
  now <- getCurrentTime
  pure . ReSchedule $ wakeAt now (length due) (catMaybes (nextAt : written))

-- | One batch: take its lock, read it again, and call HDFC once if it is still due.
--
--   The re-read is what makes a duplicated run harmless -- the list came from Postgres, which
--   lags the write path, so a batch another run has already moved on can still appear here.
--   Returns the time the step wrote (or found), so the run can sleep until then without waiting
--   for Postgres. Nothing when another run holds the lock: that run owns the batch's next time.
stepOneBatch ::
  ( ServiceFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    HasKafkaProducer r,
    Redis.HedisLTSFlowEnv r,
    JobCreator r m
  ) =>
  DPayoutBatch.PayoutBatch ->
  m (Maybe UTCTime)
stepOneBatch batch = do
  res <- Redis.whenWithLockRedisAndReturnValue ("BulkPayoutStatusCheck:batch:" <> batch.id.getId) 120 $ do
    now <- getCurrentTime
    latestBatch <- QPayoutBatch.findByPrimaryKey batch.id
    case latestBatch of
      Nothing -> do
        logError $ "Status check: batch vanished between listing and locking: " <> batch.id.getId
        pure Nothing
      Just fresh
        | maybe False (<= now) fresh.nextStatusCallAt -> advanceBatch fresh
        | otherwise -> pure fresh.nextStatusCallAt
  pure (either (const Nothing) identity res)

-- | When to run again: the earliest call anyone is owed, but never sooner than two seconds and
--   never later than the ceiling. A full page means there is more waiting, so come straight back.
wakeAt :: UTCTime -> Int -> [UTCTime] -> UTCTime
wakeAt now dueCount candidates
  | dueCount >= pageSize = soon
  | otherwise = max soon (minimum (ceiling' : candidates))
  where
    soon = addUTCTime 2 now
    ceiling' = addUTCTime BSC.statusCheckMaxSleep now
