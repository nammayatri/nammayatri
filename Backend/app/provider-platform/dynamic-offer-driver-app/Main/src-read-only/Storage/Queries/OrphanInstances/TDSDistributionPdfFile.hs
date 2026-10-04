{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.TDSDistributionPdfFile where

import qualified Domain.Types.TDSDistributionPdfFile
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.TDSDistributionPdfFile as Beam

instance FromTType' Beam.TDSDistributionPdfFile Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile where
  fromTType' (Beam.TDSDistributionPdfFileT {..}) = do
    pure $
      Just
        Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile
          { batchId = Kernel.Types.Id.Id <$> batchId,
            createdAt = createdAt,
            fileName = fileName,
            id = Kernel.Types.Id.Id id,
            issue = issue,
            matchedPersonId = Kernel.Types.Id.Id <$> matchedPersonId,
            recipientType = recipientType,
            s3FilePath = s3FilePath,
            sizeBytes = sizeBytes,
            tdsDistributionRecordId = Kernel.Types.Id.Id <$> tdsDistributionRecordId,
            updatedAt = updatedAt,
            validationStatus = validationStatus
          }

instance ToTType' Beam.TDSDistributionPdfFile Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile where
  toTType' (Domain.Types.TDSDistributionPdfFile.TDSDistributionPdfFile {..}) = do
    Beam.TDSDistributionPdfFileT
      { Beam.batchId = Kernel.Types.Id.getId <$> batchId,
        Beam.createdAt = createdAt,
        Beam.fileName = fileName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.issue = issue,
        Beam.matchedPersonId = Kernel.Types.Id.getId <$> matchedPersonId,
        Beam.recipientType = recipientType,
        Beam.s3FilePath = s3FilePath,
        Beam.sizeBytes = sizeBytes,
        Beam.tdsDistributionRecordId = Kernel.Types.Id.getId <$> tdsDistributionRecordId,
        Beam.updatedAt = updatedAt,
        Beam.validationStatus = validationStatus
      }
