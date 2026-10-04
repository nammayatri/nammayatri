{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.TDSDistributionBatchExtra where

import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.TDSDistributionBatch (TDSDistributionBatch, TDSDistributionBatchStatus (CANCELLED))
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.TDSDistributionBatch as Beam
import Storage.Queries.OrphanInstances.TDSDistributionBatch ()

-- | A city's batches that were not cancelled, newest first; optionally only one financial year.
findAllActiveByCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  m [TDSDistributionBatch]
findAllActiveByCity merchantOperatingCityId mbFinancialYear limit offset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId.getId,
          Se.Is Beam.status $ Se.Not $ Se.Eq CANCELLED
        ]
          <> [Se.Is Beam.financialYear $ Se.Eq financialYear | Just financialYear <- [mbFinancialYear]]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset
