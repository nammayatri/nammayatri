{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DocumentVerificationConfig (module Storage.Queries.DocumentVerificationConfig, module ReExport) where

import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Vehicle
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DocumentVerificationConfig as Beam
import Storage.Queries.DocumentVerificationConfigExtra as ReExport
import Storage.Queries.Transformers.DocumentVerificationConfig

create :: KvDbFlow m r => (Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  KvDbFlow m r =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig])
findAllByMerchantOpCityId limit offset (Kernel.Types.Id.Id merchantOperatingCityId) = do findAllWithOptionsKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId] (Se.Asc Beam.order) limit offset

findByPrimaryKey ::
  KvDbFlow m r =>
  (Domain.Types.DocumentVerificationConfig.DocumentType -> Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Vehicle.Category -> m (Maybe Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig))
findByPrimaryKey documentType (Kernel.Types.Id.Id merchantOperatingCityId) vehicleCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.documentType $ Se.Eq documentType,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig -> m ())
updateByPrimaryKey (Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.checkExpiry checkExpiry,
      Se.Set Beam.checkExtraction checkExtraction,
      Se.Set Beam.dependencyDocumentType dependencyDocumentType,
      Se.Set Beam.description description,
      Se.Set Beam.disableWarning disableWarning,
      Se.Set Beam.doStrictVerifcation doStrictVerifcation,
      Se.Set Beam.isDefaultEnabledOnManualVerification isDefaultEnabledOnManualVerification,
      Se.Set Beam.isDisabled isDisabled,
      Se.Set Beam.isHidden isHidden,
      Se.Set Beam.isImageValidationRequired isImageValidationRequired,
      Se.Set Beam.isMandatory isMandatory,
      Se.Set Beam.maxRetryCount maxRetryCount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.order order,
      Se.Set Beam.rcNumberPrefixList rcNumberPrefixList,
      Se.Set Beam.supportedVehicleClassesJSON (getConfigJSON supportedVehicleClasses),
      Se.Set Beam.title title,
      Se.Set Beam.vehicleClassCheckType vehicleClassCheckType,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.documentType $ Se.Eq documentType,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]
