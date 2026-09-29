{-# OPTIONS_GHC -Wno-orphans #-}

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

-- | One city's batches the status-check job still owes a call, soonest first. No status filter is
--   needed: @nextStatusCallAt@ is cleared once a batch is done, and NULL never matches.
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

-- | When the file was sent, for answers that skip 'markSubmitted' (duplicate, refusal).
updateSubmittedAt :: BeamFlow m r => UTCTime -> Id PayoutBatch -> m ()
updateSubmittedAt submittedAt batchId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.submittedAt (Just submittedAt),
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq batchId.getId]

-- | Move a batch's next status call without touching its check number or call count, so a retry
--   after our own error does not use up a planned call.
updateNextStatusCallAt :: BeamFlow m r => UTCTime -> Id PayoutBatch -> m ()
updateNextStatusCallAt nextStatusCallAt batchId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.nextStatusCallAt (Just nextStatusCallAt),
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq batchId.getId]

-- | The largest file reference used on a value date; the Redis counter seeds from it. Text order is
--   numeric order because 'nextClientRefNo' always writes six digits.
findMaxClientRefNo :: BeamFlow m r => Day -> m Int
findMaxClientRefNo executionDate = do
  rows <-
    findAllWithOptionsDb
      [Se.Is Beam.executionDate $ Se.Eq executionDate]
      (Se.Desc Beam.clientRefNo)
      (Just 1)
      Nothing
  pure $ fromMaybe 0 (listToMaybe rows >>= readMaybe . T.unpack . (.clientRefNo))

-- | Dashboard batch list for one city over an inclusive execution-date range, newest first. Sorted
--   by created_at, which follows execution date and gives offset paging a stable order.
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

-- | Correct a batch's counts once its rows are written. It is opened on the eligibility pass's
--   numbers, and the claim-time re-check may drop some of them.
updateCounts :: BeamFlow m r => Int -> HighPrecMoney -> Int -> Id PayoutBatch -> m ()
updateCounts itemCount totalAmount excludedCount batchId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.itemCount itemCount,
      Se.Set Beam.totalAmount totalAmount,
      Se.Set Beam.excludedCount excludedCount,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq batchId.getId]
