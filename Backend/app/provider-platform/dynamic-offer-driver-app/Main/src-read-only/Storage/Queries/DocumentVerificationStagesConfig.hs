{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DocumentVerificationStagesConfig where

import qualified Domain.Types.DocumentOnboardingStage
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.DocumentVerificationStagesConfig
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DocumentVerificationStagesConfig as Beam
import qualified Storage.Queries.Transformers.DocumentVerificationStagesConfig

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DocumentVerificationStagesConfig.DocumentVerificationStagesConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DocumentVerificationStagesConfig.DocumentVerificationStagesConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.DocumentVerificationStagesConfig.DocumentVerificationStagesConfig])
findAllByMerchantOpCityId limit offset merchantOperatingCityId = do findAllWithOptionsKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)] (Se.Asc Beam.order) limit offset

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.DocumentVerificationConfig.DocumentApplicableType -> Domain.Types.DocumentVerificationConfig.DocumentCategory -> Domain.Types.DocumentOnboardingStage.DocumentOnboardingStage -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.VehicleCategory.VehicleCategory -> m (Maybe Domain.Types.DocumentVerificationStagesConfig.DocumentVerificationStagesConfig))
findByPrimaryKey applicableTo documentCategory documentOnboardingStage merchantOperatingCityId vehicleCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.applicableTo $ Se.Eq applicableTo,
          Se.Is Beam.documentCategory $ Se.Eq documentCategory,
          Se.Is Beam.documentOnboardingStage $ Se.Eq documentOnboardingStage,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DocumentVerificationStagesConfig.DocumentVerificationStagesConfig -> m ())
updateByPrimaryKey (Domain.Types.DocumentVerificationStagesConfig.DocumentVerificationStagesConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.hint hint,
      Se.Set Beam.isHidden isHidden,
      Se.Set Beam.mediaJSON (Storage.Queries.Transformers.DocumentVerificationStagesConfig.mkMediaJSON media),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.order order,
      Se.Set Beam.stageDependency stageDependency,
      Se.Set Beam.title title,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.applicableTo $ Se.Eq applicableTo,
          Se.Is Beam.documentCategory $ Se.Eq documentCategory,
          Se.Is Beam.documentOnboardingStage $ Se.Eq documentOnboardingStage,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

instance FromTType' Beam.DocumentVerificationStagesConfig Domain.Types.DocumentVerificationStagesConfig.DocumentVerificationStagesConfig where
  fromTType' (Beam.DocumentVerificationStagesConfigT {..}) = do
    media' <- Storage.Queries.Transformers.DocumentVerificationStagesConfig.getMediaFromJSON mediaJSON
    pure $
      Just
        Domain.Types.DocumentVerificationStagesConfig.DocumentVerificationStagesConfig
          { applicableTo = applicableTo,
            description = description,
            documentCategory = documentCategory,
            documentOnboardingStage = documentOnboardingStage,
            hint = hint,
            isHidden = isHidden,
            media = media',
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            order = order,
            stageDependency = stageDependency,
            title = title,
            vehicleCategory = vehicleCategory,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DocumentVerificationStagesConfig Domain.Types.DocumentVerificationStagesConfig.DocumentVerificationStagesConfig where
  toTType' (Domain.Types.DocumentVerificationStagesConfig.DocumentVerificationStagesConfig {..}) = do
    Beam.DocumentVerificationStagesConfigT
      { Beam.applicableTo = applicableTo,
        Beam.description = description,
        Beam.documentCategory = documentCategory,
        Beam.documentOnboardingStage = documentOnboardingStage,
        Beam.hint = hint,
        Beam.isHidden = isHidden,
        Beam.mediaJSON = Storage.Queries.Transformers.DocumentVerificationStagesConfig.mkMediaJSON media,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.order = order,
        Beam.stageDependency = stageDependency,
        Beam.title = title,
        Beam.vehicleCategory = vehicleCategory,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
