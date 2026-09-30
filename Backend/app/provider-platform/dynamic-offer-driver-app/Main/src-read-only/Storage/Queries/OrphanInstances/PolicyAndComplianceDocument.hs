{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PolicyAndComplianceDocument where

import qualified Domain.Types.PolicyAndComplianceDocument
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PolicyAndComplianceDocument as Beam

instance FromTType' Beam.PolicyAndComplianceDocument Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument where
  fromTType' (Beam.PolicyAndComplianceDocumentT {..}) = do
    pure $
      Just
        Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument
          { createdAt = createdAt,
            enabled = enabled,
            id = Kernel.Types.Id.Id id,
            isMandatory = isMandatory,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            metadata = metadata,
            policyType = policyType,
            updatedAt = updatedAt,
            url = url,
            version = version
          }

instance ToTType' Beam.PolicyAndComplianceDocument Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument where
  toTType' (Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument {..}) = do
    Beam.PolicyAndComplianceDocumentT
      { Beam.createdAt = createdAt,
        Beam.enabled = enabled,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isMandatory = isMandatory,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.metadata = metadata,
        Beam.policyType = policyType,
        Beam.updatedAt = updatedAt,
        Beam.url = url,
        Beam.version = version
      }
