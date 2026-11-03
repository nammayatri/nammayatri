{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.TDSDistributionBatch where

import qualified Domain.Types.TDSDistributionBatch
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.TDSDistributionBatch as Beam

instance FromTType' Beam.TDSDistributionBatch Domain.Types.TDSDistributionBatch.TDSDistributionBatch where
  fromTType' (Beam.TDSDistributionBatchT {..}) = do
    pure $
      Just
        Domain.Types.TDSDistributionBatch.TDSDistributionBatch
          { completedAt = completedAt,
            confirmedAt = confirmedAt,
            confirmedById = confirmedById,
            confirmedByName = confirmedByName,
            createdAt = createdAt,
            financialYear = financialYear,
            folderName = folderName,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            quarter = quarter,
            status = status,
            totalFiles = totalFiles,
            updatedAt = updatedAt,
            uploadedById = uploadedById,
            uploadedByName = uploadedByName,
            validatedAt = validatedAt
          }

instance ToTType' Beam.TDSDistributionBatch Domain.Types.TDSDistributionBatch.TDSDistributionBatch where
  toTType' (Domain.Types.TDSDistributionBatch.TDSDistributionBatch {..}) = do
    Beam.TDSDistributionBatchT
      { Beam.completedAt = completedAt,
        Beam.confirmedAt = confirmedAt,
        Beam.confirmedById = confirmedById,
        Beam.confirmedByName = confirmedByName,
        Beam.createdAt = createdAt,
        Beam.financialYear = financialYear,
        Beam.folderName = folderName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.quarter = quarter,
        Beam.status = status,
        Beam.totalFiles = totalFiles,
        Beam.updatedAt = updatedAt,
        Beam.uploadedById = uploadedById,
        Beam.uploadedByName = uploadedByName,
        Beam.validatedAt = validatedAt
      }
