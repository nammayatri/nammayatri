{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetOwnerDocumentVerificationConfig where

import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.FleetOwnerDocumentVerificationConfig
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOwnerDocumentVerificationConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig])
findAllByMerchantOpCityId limit offset merchantOperatingCityId = do findAllWithOptionsKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)] (Se.Asc Beam.order) limit offset

findAllByMerchantOpCityIdAndRole ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Person.Role -> m [Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig])
findAllByMerchantOpCityIdAndRole merchantOperatingCityId role = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.role $ Se.Eq role
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.DocumentVerificationConfig.DocumentType -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Person.Role -> m (Maybe Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig))
findByPrimaryKey documentType merchantOperatingCityId role = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.documentType $ Se.Eq documentType,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.role $ Se.Eq role
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig -> m ())
updateByPrimaryKey (Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.checkExpiry checkExpiry,
      Se.Set Beam.checkExtraction checkExtraction,
      Se.Set Beam.dependencyDocumentType dependencyDocumentType,
      Se.Set Beam.description description,
      Se.Set Beam.disableWarning disableWarning,
      Se.Set Beam.doStrictVerifcation doStrictVerifcation,
      Se.Set Beam.documentCategory documentCategory,
      Se.Set Beam.isDefaultEnabledOnManualVerification isDefaultEnabledOnManualVerification,
      Se.Set Beam.isDisabled isDisabled,
      Se.Set Beam.isHidden isHidden,
      Se.Set Beam.isImageValidationRequired isImageValidationRequired,
      Se.Set Beam.isMandatory isMandatory,
      Se.Set Beam.maxRetryCount maxRetryCount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.order order,
      Se.Set Beam.title title,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.documentType $ Se.Eq documentType,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.role $ Se.Eq role
        ]
    ]

instance FromTType' Beam.FleetOwnerDocumentVerificationConfig Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig where
  fromTType' (Beam.FleetOwnerDocumentVerificationConfigT {..}) = do
    pure $
      Just
        Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig
          { checkExpiry = checkExpiry,
            checkExtraction = checkExtraction,
            dependencyDocumentType = dependencyDocumentType,
            description = description,
            disableWarning = disableWarning,
            doStrictVerifcation = doStrictVerifcation,
            documentCategory = documentCategory,
            documentType = documentType,
            isDefaultEnabledOnManualVerification = isDefaultEnabledOnManualVerification,
            isDisabled = isDisabled,
            isHidden = isHidden,
            isImageValidationRequired = isImageValidationRequired,
            isMandatory = isMandatory,
            maxRetryCount = maxRetryCount,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            order = order,
            role = role,
            title = title,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetOwnerDocumentVerificationConfig Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig where
  toTType' (Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig {..}) = do
    Beam.FleetOwnerDocumentVerificationConfigT
      { Beam.checkExpiry = checkExpiry,
        Beam.checkExtraction = checkExtraction,
        Beam.dependencyDocumentType = dependencyDocumentType,
        Beam.description = description,
        Beam.disableWarning = disableWarning,
        Beam.doStrictVerifcation = doStrictVerifcation,
        Beam.documentCategory = documentCategory,
        Beam.documentType = documentType,
        Beam.isDefaultEnabledOnManualVerification = isDefaultEnabledOnManualVerification,
        Beam.isDisabled = isDisabled,
        Beam.isHidden = isHidden,
        Beam.isImageValidationRequired = isImageValidationRequired,
        Beam.isMandatory = isMandatory,
        Beam.maxRetryCount = maxRetryCount,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.order = order,
        Beam.role = role,
        Beam.title = title,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
