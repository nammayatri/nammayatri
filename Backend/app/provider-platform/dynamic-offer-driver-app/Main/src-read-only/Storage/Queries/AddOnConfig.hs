{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AddOnConfig where

import qualified Data.Aeson
import qualified Domain.Types.AddOnConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Sequelize as Se
import qualified Storage.Beam.AddOnConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AddOnConfig.AddOnConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AddOnConfig.AddOnConfig] -> m ())
createMany = traverse_ create

findAllByIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Types.Id.Id Domain.Types.AddOnConfig.AddOnConfig] -> m [Domain.Types.AddOnConfig.AddOnConfig])
findAllByIds ids = do findAllWithKV [Se.Is Beam.id $ Se.In (Kernel.Types.Id.getId <$> ids)]

findAllByMerchantOpCityIdAndEnabled ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Bool -> m [Domain.Types.AddOnConfig.AddOnConfig])
findAllByMerchantOpCityIdAndEnabled merchantOperatingCityId enabled = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AddOnConfig.AddOnConfig -> m (Maybe Domain.Types.AddOnConfig.AddOnConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AddOnConfig.AddOnConfig -> m ())
updateByPrimaryKey (Domain.Types.AddOnConfig.AddOnConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.addOnType addOnType,
      Se.Set Beam.descriptorName descriptorName,
      Se.Set Beam.descriptorShortDesc descriptorShortDesc,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.maxQuantity maxQuantity,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.metadata (Data.Aeson.toJSON <$> metadata),
      Se.Set Beam.pricePerQuantity pricePerQuantity,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleServiceTier vehicleServiceTier
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.AddOnConfig Domain.Types.AddOnConfig.AddOnConfig where
  fromTType' (Beam.AddOnConfigT {..}) = do
    pure $
      Just
        Domain.Types.AddOnConfig.AddOnConfig
          { addOnType = addOnType,
            createdAt = createdAt,
            descriptorName = descriptorName,
            descriptorShortDesc = descriptorShortDesc,
            enabled = enabled,
            id = Kernel.Types.Id.Id id,
            maxQuantity = maxQuantity,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            metadata = Kernel.Utils.JSON.valueToMaybe =<< metadata,
            pricePerQuantity = pricePerQuantity,
            updatedAt = updatedAt,
            vehicleServiceTier = vehicleServiceTier
          }

instance ToTType' Beam.AddOnConfig Domain.Types.AddOnConfig.AddOnConfig where
  toTType' (Domain.Types.AddOnConfig.AddOnConfig {..}) = do
    Beam.AddOnConfigT
      { Beam.addOnType = addOnType,
        Beam.createdAt = createdAt,
        Beam.descriptorName = descriptorName,
        Beam.descriptorShortDesc = descriptorShortDesc,
        Beam.enabled = enabled,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxQuantity = maxQuantity,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.metadata = Data.Aeson.toJSON <$> metadata,
        Beam.pricePerQuantity = pricePerQuantity,
        Beam.updatedAt = updatedAt,
        Beam.vehicleServiceTier = vehicleServiceTier
      }
