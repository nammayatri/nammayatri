{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FleetOnwerDocumentVerificationConfig where

import qualified Domain.Types.FleetOnwerDocumentVerificationConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FleetOnwerDocumentVerificationConfig as Beam

instance FromTType' (Beam.FleetOnwerDocumentVerificationConfig) (Domain.Types.FleetOnwerDocumentVerificationConfig.FleetOnwerDocumentVerificationConfig) where
  fromTType' (Beam.FleetOnwerDocumentVerificationConfigT {..}) = do
    pure $
      Just
        (Domain.Types.FleetOnwerDocumentVerificationConfig.FleetOnwerDocumentVerificationConfig){checkExpiry = checkExpiry,
                                                                                                 checkExtraction = checkExtraction,
                                                                                                 dependencyDocumentType = dependencyDocumentType,
                                                                                                 description = description,
                                                                                                 disableWarning = disableWarning,
                                                                                                 documentType = documentType,
                                                                                                 isDisabled = isDisabled,
                                                                                                 isHidden = isHidden,
                                                                                                 isMandatory = isMandatory,
                                                                                                 maxRetryCount = maxRetryCount,
                                                                                                 merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                 merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                 order = order,
                                                                                                 title = title,
                                                                                                 createdAt = createdAt,
                                                                                                 updatedAt = updatedAt
                                                                                                }

instance ToTType' (Beam.FleetOnwerDocumentVerificationConfig) (Domain.Types.FleetOnwerDocumentVerificationConfig.FleetOnwerDocumentVerificationConfig) where
  toTType' (Domain.Types.FleetOnwerDocumentVerificationConfig.FleetOnwerDocumentVerificationConfig {..}) = do
    Beam.FleetOnwerDocumentVerificationConfigT
      { Beam.checkExpiry = checkExpiry,
        Beam.checkExtraction = checkExtraction,
        Beam.dependencyDocumentType = dependencyDocumentType,
        Beam.description = description,
        Beam.disableWarning = disableWarning,
        Beam.documentType = documentType,
        Beam.isDisabled = isDisabled,
        Beam.isHidden = isHidden,
        Beam.isMandatory = isMandatory,
        Beam.maxRetryCount = maxRetryCount,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.order = order,
        Beam.title = title,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
