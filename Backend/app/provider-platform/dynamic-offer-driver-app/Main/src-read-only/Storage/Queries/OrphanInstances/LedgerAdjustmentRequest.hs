{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.LedgerAdjustmentRequest where

import qualified Domain.Types.LedgerAdjustmentRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.LedgerAdjustmentRequest as Beam

instance FromTType' Beam.LedgerAdjustmentRequest Domain.Types.LedgerAdjustmentRequest.LedgerAdjustmentRequest where
  fromTType' (Beam.LedgerAdjustmentRequestT {..}) = do
    pure $
      Just
        Domain.Types.LedgerAdjustmentRequest.LedgerAdjustmentRequest
          { adminCheckerId = Kernel.Types.Id.Id <$> adminCheckerId,
            adminCheckerName = adminCheckerName,
            adminMakerId = Kernel.Types.Id.Id adminMakerId,
            adminMakerName = adminMakerName,
            amount = amount,
            approvedAt = approvedAt,
            category = category,
            currency = currency,
            description = description,
            direction = direction,
            documentId = Kernel.Types.Id.Id <$> documentId,
            errorMessage = errorMessage,
            id = Kernel.Types.Id.Id id,
            ledgerEntryId = Kernel.Types.Id.Id <$> ledgerEntryId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            postedAt = postedAt,
            referenceId = referenceId,
            referenceType = referenceType,
            status = status,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.LedgerAdjustmentRequest Domain.Types.LedgerAdjustmentRequest.LedgerAdjustmentRequest where
  toTType' (Domain.Types.LedgerAdjustmentRequest.LedgerAdjustmentRequest {..}) = do
    Beam.LedgerAdjustmentRequestT
      { Beam.adminCheckerId = Kernel.Types.Id.getId <$> adminCheckerId,
        Beam.adminCheckerName = adminCheckerName,
        Beam.adminMakerId = Kernel.Types.Id.getId adminMakerId,
        Beam.adminMakerName = adminMakerName,
        Beam.amount = amount,
        Beam.approvedAt = approvedAt,
        Beam.category = category,
        Beam.currency = currency,
        Beam.description = description,
        Beam.direction = direction,
        Beam.documentId = Kernel.Types.Id.getId <$> documentId,
        Beam.errorMessage = errorMessage,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.ledgerEntryId = Kernel.Types.Id.getId <$> ledgerEntryId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.postedAt = postedAt,
        Beam.referenceId = referenceId,
        Beam.referenceType = referenceType,
        Beam.status = status,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
