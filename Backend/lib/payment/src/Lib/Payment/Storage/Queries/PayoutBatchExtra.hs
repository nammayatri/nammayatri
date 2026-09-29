{-# OPTIONS_GHC -Wno-orphans #-}

-- NOTE (reviewer, remove before merge): NEW file, not on main -- the hand-written queries for the new
--   payout_batch table (re-exported by the generated Storage/Queries/PayoutBatch.hs). Only the bulk flow
--   writes payout_batch, so nothing here can read or change Juspay/Stripe data. Callers: Bulk.StatusCheck
--   (findDueForStatusHit), Bulk.Batch (findMaxClientRefNo), Bulk.Submit
--   (updateSubmittedAt), Bulk.Cycle (updateCounts) and the new admin batch list
--   (Domain/Action/Dashboard/PayoutBatch.listPayoutBatches -> findAllPayoutBatchesWithFilters).
module Lib.Payment.Storage.Queries.PayoutBatchExtra where

import qualified Data.Text as T
import Data.Time (Day)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Payment.Domain.Types.PayoutBatch
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PayoutBatch as Beam
import Lib.Payment.Storage.Queries.OrphanInstances.PayoutBatch ()
import qualified Sequelize as Se

-- NOTE (reviewer, remove before merge): this query takes the city (merchantOperatingCityId), because each
--   city has its own status-check job that must see only its own batches. It is the job's one list query per
--   run (the job runs again 5 s after a busy run, else after the city's interval). Served by the index
--   idx_payout_batch_moc_next_status_call_at (migration 0893). Bulk-only (only Bulk.StatusCheck calls it).

-- | One city's batches the status-check job still owes a call, soonest first.
--
--   @nextStatusCallAt@ is set exactly while a batch is owed one, so no status filter is needed:
--   a finished batch has it cleared and a NULL never satisfies the comparison.
findDueForStatusHit :: BeamFlow m r => Text -> UTCTime -> Int -> m [PayoutBatch]
findDueForStatusHit merchantOperatingCityId now limit =
  findAllWithOptionsDb
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.nextStatusCallAt $ Se.LessThanOrEq (Just now)
        ]
    ]
    (Se.Asc Beam.nextStatusCallAt)
    (Just limit)
    Nothing

-- NOTE (reviewer, remove before merge): for a file HDFC refused (duplicate, refused, gateway refusal),
--   Bulk.Submit.submitBatch saves here when we sent it. Bulk-only.

-- | When the file was sent, for the partner's answers that don't go through 'markSubmitted'
--   (a duplicate file, a refused file, a gateway refusal).
updateSubmittedAt :: BeamFlow m r => UTCTime -> Id PayoutBatch -> m ()
updateSubmittedAt submittedAt batchId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.submittedAt (Just submittedAt),
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq batchId.getId]

-- NOTE (reviewer, remove before merge): seeds the HDFC file-number counter (Bulk.Batch.nextClientRefNo)
--   when its Redis key is missing. Index idx_payout_batch_execution_date_moc (migration 0893), shared with
--   the admin batch list. Bulk-only.

-- | The largest file reference already used on a value date. The Redis counter seeds itself from
--   this when its key is missing.
--
--   That day's batches come off the @(execution_date, merchant_operating_city_id)@ index -- a day
--   holds a few hundred at most, across every city -- and the largest reference is kept. Text order
--   is numeric order here because every reference is written by 'nextClientRefNo' as exactly six
--   digits.
findMaxClientRefNo :: BeamFlow m r => Day -> m Int
findMaxClientRefNo executionDate = do
  rows <-
    findAllWithOptionsDb
      [Se.Is Beam.executionDate $ Se.Eq executionDate]
      (Se.Desc Beam.clientRefNo)
      (Just 1)
      Nothing
  pure $ fromMaybe 0 (listToMaybe rows >>= readMaybe . T.unpack . (.clientRefNo))

-- NOTE (reviewer, remove before merge): the new admin batch list (Dashboard/PayoutBatch.listPayoutBatches),
--   always filtered by the city in the URL and an execution-date range. Index
--   idx_payout_batch_execution_date_moc (migration 0893). Reads only payout_batch, so bulk-only data.

-- | Dashboard batch list for one city over an inclusive execution-date range, with optional
--   combinable filters, newest first. The date range and the city are both on the
--   (execution_date, merchant_operating_city_id) index. Newest first is by created_at: a batch's
--   execution date is the city's day it was created, so this is execution-date order with a
--   stable tie-break inside a day, which offset paging needs.
findAllPayoutBatchesWithFilters ::
  BeamFlow m r =>
  Text -> -- merchantOperatingCityId
  Day -> -- execution date from
  Day -> -- execution date to
  Maybe PayoutBatchStatus ->
  Maybe PayoutBatchOrigin ->
  Maybe PayoutBatchRail ->
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  m [PayoutBatch]
findAllPayoutBatchesWithFilters merchantOperatingCityId fromDate toDate mbStatus mbOrigin mbRail limit offset =
  findAllWithOptionsDb
    [ Se.And
        ( [ Se.Is Beam.executionDate $ Se.GreaterThanOrEq fromDate,
            Se.Is Beam.executionDate $ Se.LessThanOrEq toDate,
            Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId
          ]
            <> [Se.Is Beam.status $ Se.Eq status | Just status <- [mbStatus]]
            <> [Se.Is Beam.origin $ Se.Eq origin | Just origin <- [mbOrigin]]
            <> [Se.Is Beam.payoutRail $ Se.Eq rail | Just rail <- [mbRail]]
        )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

-- NOTE (reviewer, remove before merge): Bulk.Cycle corrects the batch's item count, total and excluded
--   count with this after the claims (runBulkCycle; people the claim-time re-check dropped never got a
--   row) and for a batch of one (openSingleBulkPayout / sendSingleBulkPayout). Bulk-only.

-- | Correct a batch's membership numbers after its rows have actually been written. The batch is
--   opened before them (so each row carries its batchId from birth), on numbers taken from the
--   eligibility pass -- anyone the claim-time re-check drops never becomes a row underneath it.
updateCounts :: BeamFlow m r => Int -> HighPrecMoney -> Int -> Id PayoutBatch -> m ()
updateCounts itemCount totalAmount excludedCount batchId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.itemCount itemCount,
      Se.Set Beam.totalAmount totalAmount,
      Se.Set Beam.excludedCount excludedCount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq batchId.getId]
