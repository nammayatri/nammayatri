{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Payment.Storage.Queries.PayoutRequestExtra where

import Data.Time (addUTCTime)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id (getId)
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

-- NOTE (reviewer, remove before merge): new query (main's queries above are unchanged). Used only by the
--   admin batch drill-down (Domain/Action/Dashboard/PayoutBatch.listPayoutBatchOrders) to list the people
--   dropped from a batch. Juspay/Stripe: no caller, and it cannot match their rows -- status EXCLUDED and
--   payout_request.batch_id (both not on main) are written only by the bulk exclusion
--   (Bulk.Batch.recordExclusion). Index idx_payout_request_excluded_moc_created_at (migration 0893), shared
--   with findExcludedByMocAndTime.

-- | Beneficiaries dropped from a batch for want of bank details. They have no payout_order --
--   nothing was submitted for them -- so the record lives on payout_request, tagged with the
--   batch it was dropped from. An exclusion is written while its batch is being claimed, so this
--   reads the city's excluded rows from the batch's creation (less a second for rounding) to a day
--   after, on the same partial index as the city worklist below; batch_id then keeps exactly this
--   batch's rows. A batch's claims finish well inside that day.
findExcludedOfBatch :: BeamFlow m r => DPayoutBatch.PayoutBatch -> m [PayoutRequest]
findExcludedOfBatch batch =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq batch.merchantOperatingCityId,
          Se.Is Beam.status $ Se.Eq EXCLUDED,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq (addUTCTime (-1) batch.createdAt),
          Se.Is Beam.createdAt $ Se.LessThanOrEq (addUTCTime 86400 batch.createdAt),
          Se.Is Beam.batchId $ Se.Eq (Just (getId batch.id))
        ]
    ]

-- NOTE (reviewer, remove before merge): new query (not on main). Used only by the admin "excluded"
--   worklist (Domain/Action/Dashboard/PayoutBatch.listPayoutExcluded), for the city in the URL.
--   Juspay/Stripe: no caller, and no Juspay/Stripe request is ever EXCLUDED, so it never returns their rows.
--   Index idx_payout_request_excluded_moc_created_at (migration 0893).

-- | Beneficiaries dropped for want of bank details in a city, newest first. The admin worklist:
--   people a human can act on by adding details, so it is scoped to EXCLUDED requests
--   and never includes an attempted-and-failed payout. An admin read, so it goes straight to the
--   database (partial index on (merchant_operating_city_id, created_at) WHERE status='EXCLUDED')
--   rather than merging KV rows into an offset page. The range is [from, to): @to@ is exclusive.
findExcludedByMocAndTime :: BeamFlow m r => Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Int -> Maybe Int -> m [PayoutRequest]
findExcludedByMocAndTime mocId mbFrom mbTo limit offset =
  findAllWithOptionsDb
    [ Se.And
        ( [ Se.Is Beam.merchantOperatingCityId $ Se.Eq mocId,
            Se.Is Beam.status $ Se.Eq EXCLUDED
          ]
            <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq (fromJust mbFrom) | isJust mbFrom]
            <> [Se.Is Beam.createdAt $ Se.LessThan (fromJust mbTo) | isJust mbTo]
        )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset
