{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SurgeConfig where

import qualified Domain.Types.Common
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SurgeConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Text
import qualified Sequelize as Se
import qualified Storage.Beam.SurgeConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SurgeConfig.SurgeConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SurgeConfig.SurgeConfig] -> m ())
createMany = traverse_ create

findAllByCityAndServiceTier ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Common.ServiceTierType -> m [Domain.Types.SurgeConfig.SurgeConfig])
findAllByCityAndServiceTier merchantOperatingCityId vehicleServiceTier = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleServiceTier $ Se.Eq vehicleServiceTier
        ]
    ]

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.SurgeConfig.SurgeConfig])
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

updateStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SurgeConfig.SurgeConfigStatus -> Kernel.Types.Id.Id Domain.Types.SurgeConfig.SurgeConfig -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SurgeConfig.SurgeConfig -> m (Maybe Domain.Types.SurgeConfig.SurgeConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SurgeConfig.SurgeConfig -> m ())
updateByPrimaryKey (Domain.Types.SurgeConfig.SurgeConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.applyOnExtraDistanceOnly applyOnExtraDistanceOnly,
      Se.Set Beam.createdBy createdBy,
      Se.Set Beam.description description,
      Se.Set Beam.excludedAreas (Kernel.Utils.Text.encodeToText <$> excludedAreas),
      Se.Set Beam.maxDeltaPerUpdate maxDeltaPerUpdate,
      Se.Set Beam.maxMultiplier maxMultiplier,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.minMultiplier minMultiplier,
      Se.Set Beam.rows (Kernel.Utils.Text.encodeToText rows),
      Se.Set Beam.status status,
      Se.Set Beam.timeBounds timeBounds,
      Se.Set Beam.vehicleServiceTier vehicleServiceTier,
      Se.Set Beam.version version,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SurgeConfig Domain.Types.SurgeConfig.SurgeConfig where
  fromTType' (Beam.SurgeConfigT {..}) = do
    pure $
      Just
        Domain.Types.SurgeConfig.SurgeConfig
          { applyOnExtraDistanceOnly = applyOnExtraDistanceOnly,
            createdBy = createdBy,
            description = description,
            excludedAreas = excludedAreas >>= Kernel.Utils.Text.decodeFromText,
            id = Kernel.Types.Id.Id id,
            maxDeltaPerUpdate = maxDeltaPerUpdate,
            maxMultiplier = maxMultiplier,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            minMultiplier = minMultiplier,
            rows = fromMaybe [] (Kernel.Utils.Text.decodeFromText rows),
            status = status,
            timeBounds = timeBounds,
            vehicleServiceTier = vehicleServiceTier,
            version = version,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SurgeConfig Domain.Types.SurgeConfig.SurgeConfig where
  toTType' (Domain.Types.SurgeConfig.SurgeConfig {..}) = do
    Beam.SurgeConfigT
      { Beam.applyOnExtraDistanceOnly = applyOnExtraDistanceOnly,
        Beam.createdBy = createdBy,
        Beam.description = description,
        Beam.excludedAreas = Kernel.Utils.Text.encodeToText <$> excludedAreas,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxDeltaPerUpdate = maxDeltaPerUpdate,
        Beam.maxMultiplier = maxMultiplier,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.minMultiplier = minMultiplier,
        Beam.rows = Kernel.Utils.Text.encodeToText rows,
        Beam.status = status,
        Beam.timeBounds = timeBounds,
        Beam.vehicleServiceTier = vehicleServiceTier,
        Beam.version = version,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
