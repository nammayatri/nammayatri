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

-- | Batches the status-check job still owes a call, soonest first.
--
--   @nextStatusCallAt@ is set exactly while a batch is owed one, so no status filter is needed:
--   a finished batch has it cleared and a NULL never satisfies the comparison.
findDueForStatusHit :: BeamFlow m r => UTCTime -> Int -> m [PayoutBatch]
findDueForStatusHit now limit =
  findAllWithOptionsDb
    [Se.Is Beam.nextStatusCallAt $ Se.LessThanOrEq (Just now)]
    (Se.Asc Beam.nextStatusCallAt)
    (Just limit)
    Nothing

-- | When the next call of any batch falls due, so the job can sleep until then.
findEarliestNextStatusHitAt :: BeamFlow m r => m (Maybe UTCTime)
findEarliestNextStatusHitAt = do
  now <- getCurrentTime
  -- An upper bound rather than "is not null": it reads the same index and keeps the NULL
  -- handling in SQL's comparison rules, where a NULL simply never matches.
  let farFuture = addUTCTime (400 * 86400) now
  rows <-
    findAllWithOptionsDb
      [Se.Is Beam.nextStatusCallAt $ Se.LessThanOrEq (Just farFuture)]
      (Se.Asc Beam.nextStatusCallAt)
      (Just 1)
      Nothing
  pure (listToMaybe rows >>= (.nextStatusCallAt))

-- | The largest file reference already used on a value date. The Redis counter seeds itself from
--   this when its key is missing.
--
--   One row, read off the @(execution_date, client_ref_no)@ index. Text order is numeric order here
--   because every reference is written by 'nextClientRefNo' as exactly six digits.
findMaxClientRefNo :: BeamFlow m r => Day -> m Int
findMaxClientRefNo executionDate = do
  rows <-
    findAllWithOptionsDb
      [Se.Is Beam.executionDate $ Se.Eq executionDate]
      (Se.Desc Beam.clientRefNo)
      (Just 1)
      Nothing
  pure $ fromMaybe 0 (listToMaybe rows >>= readMaybe . T.unpack . (.clientRefNo))

-- | Dashboard batch list with optional/combinable filters, newest first. The city implies the
--   merchant, so filtering on it alone keeps the query on the (merchant_operating_city_id, created_at)
--   index.
findAllPayoutBatchesWithFilters ::
  BeamFlow m r =>
  Text -> -- merchantOperatingCityId
  Maybe UTCTime -> -- from
  Maybe UTCTime -> -- to
  Maybe PayoutBatchStatus ->
  Maybe PayoutBatchOrigin ->
  Maybe PayoutBatchRail ->
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  m [PayoutBatch]
findAllPayoutBatchesWithFilters merchantOperatingCityId mbFrom mbTo mbStatus mbOrigin mbRail limit offset =
  findAllWithOptionsDb
    [ Se.And
        ( [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]
            <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq from | Just from <- [mbFrom]]
            <> [Se.Is Beam.createdAt $ Se.LessThanOrEq to | Just to <- [mbTo]]
            <> [Se.Is Beam.status $ Se.Eq status | Just status <- [mbStatus]]
            <> [Se.Is Beam.origin $ Se.Eq origin | Just origin <- [mbOrigin]]
            <> [Se.Is Beam.payoutRail $ Se.Eq rail | Just rail <- [mbRail]]
        )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

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
