{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VasBannerConfig where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VasBannerConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VasBannerConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VasBannerConfig.VasBannerConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VasBannerConfig.VasBannerConfig] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VasBannerConfig.VasBannerConfig -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.VasBannerConfig.VasBannerConfig]))
findAllByMerchantOperatingCityId limit offset merchantOperatingCityId = do findAllWithOptionsKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)] (Se.Asc Beam.priority) limit offset

findAllEnabledByCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Bool -> m ([Domain.Types.VasBannerConfig.VasBannerConfig]))
findAllEnabledByCity limit offset merchantOperatingCityId enabled = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
    ]
    (Se.Asc Beam.priority)
    limit
    offset

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VasBannerConfig.VasBannerConfig -> m (Maybe Domain.Types.VasBannerConfig.VasBannerConfig))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VasBannerConfig.VasBannerConfig -> m (Maybe Domain.Types.VasBannerConfig.VasBannerConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VasBannerConfig.VasBannerConfig -> m ())
updateByPrimaryKey (Domain.Types.VasBannerConfig.VasBannerConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.deepLink deepLink,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.imageUrl imageUrl,
      Se.Set Beam.linkType linkType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.priority priority,
      Se.Set Beam.subtitle subtitle,
      Se.Set Beam.title title,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validFrom validFrom,
      Se.Set Beam.validTo validTo,
      Se.Set Beam.whatsappTemplateId whatsappTemplateId
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.VasBannerConfig Domain.Types.VasBannerConfig.VasBannerConfig where
  fromTType' (Beam.VasBannerConfigT {..}) = do
    pure $
      Just
        Domain.Types.VasBannerConfig.VasBannerConfig
          { createdAt = createdAt,
            deepLink = deepLink,
            enabled = enabled,
            id = Kernel.Types.Id.Id id,
            imageUrl = imageUrl,
            linkType = linkType,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            priority = priority,
            subtitle = subtitle,
            title = title,
            updatedAt = updatedAt,
            validFrom = validFrom,
            validTo = validTo,
            whatsappTemplateId = whatsappTemplateId
          }

instance ToTType' Beam.VasBannerConfig Domain.Types.VasBannerConfig.VasBannerConfig where
  toTType' (Domain.Types.VasBannerConfig.VasBannerConfig {..}) = do
    Beam.VasBannerConfigT
      { Beam.createdAt = createdAt,
        Beam.deepLink = deepLink,
        Beam.enabled = enabled,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.imageUrl = imageUrl,
        Beam.linkType = linkType,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.priority = priority,
        Beam.subtitle = subtitle,
        Beam.title = title,
        Beam.updatedAt = updatedAt,
        Beam.validFrom = validFrom,
        Beam.validTo = validTo,
        Beam.whatsappTemplateId = whatsappTemplateId
      }
