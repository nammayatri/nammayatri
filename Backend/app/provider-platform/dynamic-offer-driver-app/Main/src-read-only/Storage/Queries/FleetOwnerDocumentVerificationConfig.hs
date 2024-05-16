{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetOwnerDocumentVerificationConfig where

import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.FleetOwnerDocumentVerificationConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOwnerDocumentVerificationConfig as Beam

create :: KvDbFlow m r => (Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  KvDbFlow m r =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig])
findAllByMerchantOpCityId limit offset (Kernel.Types.Id.Id merchantOperatingCityId) = do findAllWithOptionsKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId] (Se.Asc Beam.order) limit offset

findByPrimaryKey ::
  KvDbFlow m r =>
  (Domain.Types.DocumentVerificationConfig.DocumentType -> Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig))
findByPrimaryKey documentType (Kernel.Types.Id.Id merchantOperatingCityId) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.documentType $ Se.Eq documentType,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId
        ]
    ]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig -> m ())
updateByPrimaryKey (Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.checkExpiry checkExpiry,
      Se.Set Beam.checkExtraction checkExtraction,
      Se.Set Beam.dependencyDocumentType dependencyDocumentType,
      Se.Set Beam.description description,
      Se.Set Beam.disableWarning disableWarning,
      Se.Set Beam.isDisabled isDisabled,
      Se.Set Beam.isHidden isHidden,
      Se.Set Beam.isMandatory isMandatory,
      Se.Set Beam.maxRetryCount maxRetryCount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.order order,
      Se.Set Beam.title title,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.documentType $ Se.Eq documentType,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
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

instance ToTType' Beam.FleetOwnerDocumentVerificationConfig Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig where
  toTType' (Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig {..}) = do
    Beam.FleetOwnerDocumentVerificationConfigT
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
