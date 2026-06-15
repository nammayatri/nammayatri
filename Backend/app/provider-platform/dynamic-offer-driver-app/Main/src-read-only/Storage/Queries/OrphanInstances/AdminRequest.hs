{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.AdminRequest where

import qualified Domain.Types.AdminRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.AdminRequest as Beam

instance FromTType' Beam.AdminRequest Domain.Types.AdminRequest.AdminRequest where
  fromTType' (Beam.AdminRequestT {..}) = do
    pure $
      Just
        Domain.Types.AdminRequest.AdminRequest
          { actionType = actionType,
            adjustmentType = adjustmentType,
            adminCheckerId = Kernel.Types.Id.Id <$> adminCheckerId,
            adminCheckerName = adminCheckerName,
            adminMakerId = Kernel.Types.Id.Id adminMakerId,
            adminMakerName = adminMakerName,
            amount = amount,
            currency = currency,
            description = description,
            errorMessage = errorMessage,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            referenceId = referenceId,
            referenceTable = referenceTable,
            referenceType = referenceType,
            source = source,
            status = status,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AdminRequest Domain.Types.AdminRequest.AdminRequest where
  toTType' (Domain.Types.AdminRequest.AdminRequest {..}) = do
    Beam.AdminRequestT
      { Beam.actionType = actionType,
        Beam.adjustmentType = adjustmentType,
        Beam.adminCheckerId = Kernel.Types.Id.getId <$> adminCheckerId,
        Beam.adminCheckerName = adminCheckerName,
        Beam.adminMakerId = Kernel.Types.Id.getId adminMakerId,
        Beam.adminMakerName = adminMakerName,
        Beam.amount = amount,
        Beam.currency = currency,
        Beam.description = description,
        Beam.errorMessage = errorMessage,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.referenceId = referenceId,
        Beam.referenceTable = referenceTable,
        Beam.referenceType = referenceType,
        Beam.source = source,
        Beam.status = status,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
