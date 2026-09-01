{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.Payout.BulkBatchPayoutPoll (sendBulkBatchPayoutPoll) where

import qualified Domain.Types.PayoutRun as DDPR
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.Payout.ScheduledBatchPayout (reconcileBulkBatches)
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.PayoutRun as QPayoutRun

-- | Polls a single HDFC CBX payout_run's batches to resolution -- shared by both the scheduled
--   sweep (whose own job reschedules to the next run long before HDFC resolves anything) and the
--   adhoc flow (which has no other job driving it at all). Reuses reconcileBulkBatches unchanged.
sendBulkBatchPayoutPoll ::
  ( ServiceFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    HasKafkaProducer r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Job 'BulkBatchPayoutPoll ->
  m ExecutionResult
sendBulkBatchPayoutPoll Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jd = jobInfo.jobData
  reconcileBulkBatches jd.payoutServiceName jd.merchantId jd.merchantOperatingCityId jd.runId.getId jd.payoutRail
  batches <- QPayoutBatch.findAllByRunId (Just jd.runId.getId)
  now <- getCurrentTime
  let allTerminal = all (\b -> b.status `elem` [DPayoutBatch.COMPLETED, DPayoutBatch.REJECTED]) batches
  if allTerminal
    then do
      sealRun jd.runId batches
      pure Complete
    else
      if now >= jd.deadline
        then do
          -- Give up polling; leave state as-is for manual reconciliation via the existing
          -- GET /payout/order/{id} and GET /payout/history dashboard endpoints.
          logWarning $ "BulkBatchPayoutPoll: deadline reached for run " <> jd.runId.getId <> ", stopping with batches still in flight"
          QPayoutRun.updateStatus DDPR.PARTIALLY_RESOLVED jd.runId
          pure Complete
        else pure $ ReSchedule (addUTCTime (30 * 60) now)

-- | Aggregate each batch's own counts/amounts into the run's resolution summary. Amounts are an
--   aggregate approximation off payout_batch's per-batch totals, not per-item precision -- the
--   authoritative per-driver numbers live on the payout_order rows themselves.
sealRun ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DDPR.PayoutRun ->
  [DPayoutBatch.PayoutBatch] ->
  m ()
sealRun runId batches = do
  now <- getCurrentTime
  let paidCount = sum (map (.processedCount) batches)
      failedCount = sum (map (.rejectedCount) batches)
      paidAmount = sum [b.totalAmount | b <- batches, b.status == DPayoutBatch.COMPLETED]
      failedAmount = sum [b.totalAmount | b <- batches, b.status == DPayoutBatch.REJECTED]
      status = if any ((== DPayoutBatch.REJECTED) . (.status)) batches then DDPR.PARTIALLY_RESOLVED else DDPR.COMPLETED
  QPayoutRun.updateResolutionCounts status paidCount failedCount 0 paidAmount failedAmount paidAmount (Just now) runId
