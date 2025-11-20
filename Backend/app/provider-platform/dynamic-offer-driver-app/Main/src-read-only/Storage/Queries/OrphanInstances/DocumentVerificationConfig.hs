{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DocumentVerificationConfig where

import qualified Data.Aeson
import qualified Domain.Types.DocumentVerificationConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
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
          { allowLicenseTransfer = allowLicenseTransfer,
            applicableTo = fromMaybe Domain.Types.DocumentVerificationConfig.FLEET_AND_INDIVIDUAL applicableTo,
            checkExpiry = checkExpiry,
            checkExtraction = checkExtraction,
            dependencyDocumentType = dependencyDocumentType,
            description = description,
            disableWarning = disableWarning,
            doStrictVerifcation = doStrictVerifcation,
            documentCategory = documentCategory,
            documentFields = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< documentFieldsJSON,
            documentType = documentType,
            filterForOldApks = filterForOldApks,
            isDefaultEnabledOnManualVerification = isDefaultEnabledOnManualVerification,
            isDisabled = isDisabled,
            isHidden = isHidden,
            isImageValidationRequired = isImageValidationRequired,
            isMandatory = isMandatory,
            isMandatoryForEnabling = isMandatoryForEnabling,
            maxRetryCount = maxRetryCount,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            order = order,
            rcNumberPrefixList = rcNumberPrefixList,
            supportedVehicleClasses = supportedVehicleClasses',
            title = title,
            vehicleCategory = vehicleCategory,
            vehicleClassCheckType = vehicleClassCheckType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DocumentVerificationConfig Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig where
  toTType' (Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig {..}) = do
    Beam.DocumentVerificationConfigT
      { Beam.allowLicenseTransfer = allowLicenseTransfer,
        Beam.applicableTo = Kernel.Prelude.Just applicableTo,
        Beam.checkExpiry = checkExpiry,
        Beam.checkExtraction = checkExtraction,
        Beam.dependencyDocumentType = dependencyDocumentType,
        Beam.description = description,
        Beam.disableWarning = disableWarning,
        Beam.doStrictVerifcation = doStrictVerifcation,
        Beam.documentCategory = documentCategory,
        Beam.documentFieldsJSON = Data.Aeson.toJSON <$> documentFields,
        Beam.documentType = documentType,
        Beam.filterForOldApks = filterForOldApks,
        Beam.isDefaultEnabledOnManualVerification = isDefaultEnabledOnManualVerification,
        Beam.isDisabled = isDisabled,
        Beam.isHidden = isHidden,
        Beam.isImageValidationRequired = isImageValidationRequired,
        Beam.isMandatory = isMandatory,
        Beam.isMandatoryForEnabling = isMandatoryForEnabling,
        Beam.maxRetryCount = maxRetryCount,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.order = order,
        Beam.rcNumberPrefixList = rcNumberPrefixList,
        Beam.supportedVehicleClassesJSON = getConfigJSON supportedVehicleClasses,
        Beam.title = title,
        Beam.vehicleCategory = vehicleCategory,
        Beam.vehicleClassCheckType = vehicleClassCheckType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
