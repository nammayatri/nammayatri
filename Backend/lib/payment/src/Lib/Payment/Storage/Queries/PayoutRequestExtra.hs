{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Payment.Storage.Queries.PayoutRequestExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
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
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  m [PayoutRequest]
findByBeneficiaryWithFilters beneficiaryId mbFrom mbTo statuses limit offset = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is Beam.beneficiaryId $ Se.Eq beneficiaryId]
            <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq (fromJust mbFrom) | isJust mbFrom]
            <> [Se.Is Beam.createdAt $ Se.LessThanOrEq (fromJust mbTo) | isJust mbTo]
            <> [Se.Is Beam.status (Se.In statuses) | not (null statuses)]
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

-- | Beneficiaries dropped from a batch for want of bank details. They have no payout_order --
--   nothing was submitted for them -- so the record lives on payout_request, tagged with the
--   batch it was dropped from. Backed by the partial index on (batch_id) WHERE status='EXCLUDED'.
findExcludedByBatchId :: BeamFlow m r => Text -> m [PayoutRequest]
findExcludedByBatchId batchId =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.batchId $ Se.Eq (Just batchId),
          Se.Is Beam.status $ Se.Eq EXCLUDED
        ]
    ]

-- | Beneficiaries dropped for want of bank details in a city, newest first. The admin worklist
--   (doc p.39): people a human can act on by adding details, so it is scoped to EXCLUDED requests
--   and never includes an attempted-and-failed payout. An admin read, so it goes straight to the
--   database (partial index on (merchant_operating_city_id, created_at) WHERE status='EXCLUDED')
--   rather than merging KV rows into an offset page.
findExcludedByMocAndTime :: BeamFlow m r => Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Int -> Maybe Int -> m [PayoutRequest]
findExcludedByMocAndTime mocId mbFrom mbTo limit offset =
  findAllWithOptionsDb
    [ Se.And
        ( [ Se.Is Beam.merchantOperatingCityId $ Se.Eq mocId,
            Se.Is Beam.status $ Se.Eq EXCLUDED
          ]
            <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq (fromJust mbFrom) | isJust mbFrom]
            <> [Se.Is Beam.createdAt $ Se.LessThanOrEq (fromJust mbTo) | isJust mbTo]
        )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset
