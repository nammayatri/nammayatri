{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetOwnerDocumentVerificationConfig where

import qualified Data.Text
import qualified Domain.Types.DocumentOnboardingStage
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.FleetOwnerDocumentVerificationConfig
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOwnerDocumentVerificationConfig as Beam
import qualified Storage.Queries.Transformers.DocumentVerificationConfig

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig]))
findAllByMerchantOpCityId limit offset merchantOperatingCityId = do findAllWithOptionsKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)] (Se.Asc Beam.order) limit offset

findAllByMerchantOpCityIdAndRole ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Person.Role -> m ([Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig]))
findAllByMerchantOpCityIdAndRole merchantOperatingCityId role = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.role $ Se.Eq role
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.DocumentOnboardingStage.DocumentOnboardingStage -> Domain.Types.DocumentVerificationConfig.DocumentType -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Person.Role -> m (Maybe Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig))
findByPrimaryKey documentOnboardingStage documentType merchantOperatingCityId role = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.documentOnboardingStage $ Se.Eq documentOnboardingStage,
          Se.Is Beam.documentType $ Se.Eq documentType,
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
      Se.Set Beam.documentFieldsJSON (Storage.Queries.Transformers.DocumentVerificationConfig.mkDocumentFieldsJSON documentFields),
      Se.Set Beam.isDefaultEnabledOnManualVerification isDefaultEnabledOnManualVerification,
      Se.Set Beam.isDisabled isDisabled,
      Se.Set Beam.isHidden isHidden,
      Se.Set Beam.isImageValidationRequired isImageValidationRequired,
      Se.Set Beam.isMandatory isMandatory,
      Se.Set Beam.isMandatoryForEnabling isMandatoryForEnabling,
      Se.Set Beam.markImageValidOnValidationSkip markImageValidOnValidationSkip,
      Se.Set Beam.maxRetryCount maxRetryCount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.onlyImageVerificationStatusLookupRequired onlyImageVerificationStatusLookupRequired,
      Se.Set Beam.order order,
      Se.Set Beam.rolesAllowedToUploadDocumentText (((Kernel.Prelude.map (Data.Text.pack . Kernel.Prelude.show)) Kernel.Prelude.<$> rolesAllowedToUploadDocument)),
      Se.Set Beam.title title,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.documentOnboardingStage $ Se.Eq documentOnboardingStage,
          Se.Is Beam.documentType $ Se.Eq documentType,
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
            documentFields = Storage.Queries.Transformers.DocumentVerificationConfig.getDocumentFieldsFromJSON documentFieldsJSON,
            documentOnboardingStage = documentOnboardingStage,
            documentType = documentType,
            isDefaultEnabledOnManualVerification = isDefaultEnabledOnManualVerification,
            isDisabled = isDisabled,
            isHidden = isHidden,
            isImageValidationRequired = isImageValidationRequired,
            isMandatory = isMandatory,
            isMandatoryForEnabling = isMandatoryForEnabling,
            markImageValidOnValidationSkip = markImageValidOnValidationSkip,
            maxRetryCount = maxRetryCount,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            onlyImageVerificationStatusLookupRequired = onlyImageVerificationStatusLookupRequired,
            order = order,
            role = role,
            rolesAllowedToUploadDocument = (rolesAllowedToUploadDocumentText >>= traverse (readMaybe . Data.Text.unpack)),
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
        Beam.documentFieldsJSON = Storage.Queries.Transformers.DocumentVerificationConfig.mkDocumentFieldsJSON documentFields,
        Beam.documentOnboardingStage = documentOnboardingStage,
        Beam.documentType = documentType,
        Beam.isDefaultEnabledOnManualVerification = isDefaultEnabledOnManualVerification,
        Beam.isDisabled = isDisabled,
        Beam.isHidden = isHidden,
        Beam.isImageValidationRequired = isImageValidationRequired,
        Beam.isMandatory = isMandatory,
        Beam.isMandatoryForEnabling = isMandatoryForEnabling,
        Beam.markImageValidOnValidationSkip = markImageValidOnValidationSkip,
        Beam.maxRetryCount = maxRetryCount,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.onlyImageVerificationStatusLookupRequired = onlyImageVerificationStatusLookupRequired,
        Beam.order = order,
        Beam.role = role,
        Beam.rolesAllowedToUploadDocumentText = ((Kernel.Prelude.map (Data.Text.pack . Kernel.Prelude.show)) Kernel.Prelude.<$> rolesAllowedToUploadDocument),
        Beam.title = title,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
