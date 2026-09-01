{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Payment.Storage.Queries.PayoutBatchExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Lib.Payment.Domain.Types.PayoutBatch
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PayoutBatch as Beam
import Lib.Payment.Storage.Queries.OrphanInstances.PayoutBatch ()
import qualified Sequelize as Se

-- | Dashboard batch list with optional/combinable filters, sorted by createdAt desc.
findAllPayoutBatchesWithFilters ::
  BeamFlow m r =>
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  Maybe UTCTime -> -- from
  Maybe UTCTime -> -- to
  Maybe PayoutBatchStatus ->
  Maybe PayoutBatchOrigin ->
  Maybe Text -> -- payoutRail
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  m [PayoutBatch]
findAllPayoutBatchesWithFilters merchantId merchantOperatingCityId mbFrom mbTo mbStatus mbOrigin mbRail limit offset =
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is Beam.merchantId $ Se.Eq merchantId]
            <> [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just merchantOperatingCityId)]
            <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq (fromJust mbFrom) | isJust mbFrom]
            <> [Se.Is Beam.createdAt $ Se.LessThanOrEq (fromJust mbTo) | isJust mbTo]
            <> [Se.Is Beam.status $ Se.Eq (fromJust mbStatus) | isJust mbStatus]
            <> [Se.Is Beam.origin $ Se.Eq (fromJust mbOrigin) | isJust mbOrigin]
            <> [Se.Is Beam.payoutRail $ Se.Eq (fromJust mbRail) | isJust mbRail]
        )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset
