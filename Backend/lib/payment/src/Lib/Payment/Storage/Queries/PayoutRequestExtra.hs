{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Payment.Storage.Queries.PayoutRequestExtra where

import Control.Lens ((^..), _Just, to)
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
            <> (mbFrom ^.. _Just . to (\v -> Se.Is Beam.createdAt $ Se.GreaterThanOrEq v))
            <> (mbTo ^.. _Just . to (\v -> Se.Is Beam.createdAt $ Se.LessThanOrEq v))
            <> [Se.Is Beam.status (Se.In statuses) | not (null statuses)]
        )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset
