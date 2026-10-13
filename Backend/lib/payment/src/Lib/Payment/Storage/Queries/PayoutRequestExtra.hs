{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Payment.Storage.Queries.PayoutRequestExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id (Id, getId)
import Kernel.Utils.Common (getCurrentTime)
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import Lib.Payment.Domain.Types.PayoutRequest
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PayoutRequest as Beam
import Lib.Payment.Storage.Queries.PayoutRequest ()
import qualified Sequelize as Se

-- | Find payout requests by beneficiary (person ID) with optional filters.
--   Results sorted by createdAt descending.
findByBeneficiaryWithFilters ::
  BeamFlow m r =>
  Text -> -- beneficiaryId
  Maybe UTCTime -> -- from
  Maybe UTCTime -> -- to
  [PayoutRequestStatus] -> -- status filter (empty = all)
  [PayoutRequestStatus] -> -- statuses to leave out (empty = none)
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  m [PayoutRequest]
findByBeneficiaryWithFilters beneficiaryId mbFrom mbTo statuses excludedStatuses limit offset = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is Beam.beneficiaryId $ Se.Eq beneficiaryId]
            <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq (fromJust mbFrom) | isJust mbFrom]
            <> [Se.Is Beam.createdAt $ Se.LessThanOrEq (fromJust mbTo) | isJust mbTo]
            <> [Se.Is Beam.status (Se.In statuses) | not (null statuses)]
            <> [Se.Is Beam.status $ Se.Not $ Se.In excludedStatuses | not (null excludedStatuses)]
        )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

-- | Bulk shape used by the reconciliation framework: fetch every
--   payout_request whose id is in the given set. Replaces per-id
--   findById loops in the recipe fetchers.
findByIds :: BeamFlow m r => [Text] -> m [PayoutRequest]
findByIds [] = pure []
findByIds prIds = findAllWithKV [Se.Is Beam.id $ Se.In prIds]

-- | Beneficiaries dropped from a batch for missing bank details. Nothing was sent for them, so
--   they exist only as EXCLUDED requests tagged with the batch.
findExcludedOfBatch :: BeamFlow m r => DPayoutBatch.PayoutBatch -> m [PayoutRequest]
findExcludedOfBatch batch =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.batchId $ Se.Eq (Just (getId batch.id)),
          Se.Is Beam.status $ Se.Eq EXCLUDED
        ]
    ]

-- | Admin worklist of a city's beneficiaries excluded for missing bank details, newest first. Read
--   straight from the DB (partial index on EXCLUDED) so offset paging is stable. @to@ is exclusive.
findExcludedByMocAndTime :: BeamFlow m r => Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Int -> Maybe Int -> m [PayoutRequest]
findExcludedByMocAndTime mocId mbFrom mbTo limit offset =
  findAllWithOptionsDb
    [ Se.And
        ( [ Se.Is Beam.merchantOperatingCityId $ Se.Eq mocId,
            Se.Is Beam.status $ Se.Eq EXCLUDED
          ]
            <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq fromTime | Just fromTime <- [mbFrom]]
            <> [Se.Is Beam.createdAt $ Se.LessThan toTime | Just toTime <- [mbTo]]
        )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

-- | Set the status only while the request is still INITIATED.
updateStatusIfInitiated :: BeamFlow m r => PayoutRequestStatus -> Id PayoutRequest -> m ()
updateStatusIfInitiated newStatus payoutRequestId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status newStatus,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (getId payoutRequestId),
          Se.Is Beam.status $ Se.Eq INITIATED
        ]
    ]
