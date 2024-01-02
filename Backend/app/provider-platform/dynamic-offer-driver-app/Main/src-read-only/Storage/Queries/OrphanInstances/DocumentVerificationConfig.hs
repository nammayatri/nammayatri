{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DocumentVerificationConfig where

import qualified Domain.Types.DocumentVerificationConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DocumentVerificationConfig as Beam
import Storage.Queries.Transformers.DocumentVerificationConfig

instance FromTType' Beam.DocumentVerificationConfig Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig where
  fromTType' (Beam.DocumentVerificationConfigT {..}) = do
    supportedVehicleClasses' <- getConfigFromJSON documentType supportedVehicleClassesJSON
    pure $
      Just
        Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig
          { checkExpiry = checkExpiry,
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
            rcNumberPrefixList = rcNumberPrefixList,
            supportedVehicleClasses = supportedVehicleClasses',
            dlNumberVerification = dlNumberVerification,
            title = title,
            vehicleCategory = vehicleCategory,
            vehicleClassCheckType = vehicleClassCheckType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DocumentVerificationConfig Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig where
  toTType' (Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig {..}) = do
    Beam.DocumentVerificationConfigT
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
        Beam.dlNumberVerification = dlNumberVerification,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.rcNumberPrefixList = rcNumberPrefixList,
        Beam.supportedVehicleClassesJSON = getConfigJSON supportedVehicleClasses,
        Beam.title = title,
        Beam.vehicleCategory = vehicleCategory,
        Beam.vehicleClassCheckType = vehicleClassCheckType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
