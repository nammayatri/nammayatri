{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DocumentVerificationConfig (module Storage.Queries.DocumentVerificationConfig, module ReExport) where

import qualified Data.Aeson
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DocumentVerificationConfig as Beam
import Storage.Queries.DocumentVerificationConfigExtra as ReExport
import Storage.Queries.Transformers.DocumentVerificationConfig

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig])
findAllByMerchantOpCityId limit offset merchantOperatingCityId = do findAllWithOptionsKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)] (Se.Asc Beam.order) limit offset

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.DocumentVerificationConfig.DocumentType -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.VehicleCategory.VehicleCategory -> m (Maybe Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig))
findByPrimaryKey documentType merchantOperatingCityId vehicleCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.documentType $ Se.Eq documentType,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig -> m ())
updateByPrimaryKey (Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowLicenseTransfer allowLicenseTransfer,
      Se.Set Beam.applicableTo (Kernel.Prelude.Just applicableTo),
      Se.Set Beam.checkExpiry checkExpiry,
      Se.Set Beam.checkExtraction checkExtraction,
      Se.Set Beam.dependencyDocumentType dependencyDocumentType,
      Se.Set Beam.description description,
      Se.Set Beam.disableWarning disableWarning,
      Se.Set Beam.doStrictVerifcation doStrictVerifcation,
      Se.Set Beam.documentCategory documentCategory,
      Se.Set Beam.documentFieldsJSON (Data.Aeson.toJSON <$> documentFields),
      Se.Set Beam.filterForOldApks filterForOldApks,
      Se.Set Beam.isDefaultEnabledOnManualVerification isDefaultEnabledOnManualVerification,
      Se.Set Beam.isDisabled isDisabled,
      Se.Set Beam.isHidden isHidden,
      Se.Set Beam.isImageValidationRequired isImageValidationRequired,
      Se.Set Beam.isMandatory isMandatory,
      Se.Set Beam.isMandatoryForEnabling isMandatoryForEnabling,
      Se.Set Beam.maxRetryCount maxRetryCount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.order order,
      Se.Set Beam.rcNumberPrefixList rcNumberPrefixList,
      Se.Set Beam.supportedVehicleClassesJSON (getConfigJSON supportedVehicleClasses),
      Se.Set Beam.title title,
      Se.Set Beam.vehicleClassCheckType vehicleClassCheckType,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.documentType $ Se.Eq documentType,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]
