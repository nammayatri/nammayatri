{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MultiModalConfigs where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MultiModalConfigs
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MultiModalConfigs as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MultiModalConfigs.MultiModalConfigs -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MultiModalConfigs.MultiModalConfigs] -> m ())
createMany = traverse_ create

findByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.MultiModalConfigs.MultiModalConfigs))
findByMerchantOperatingCityId merchantOperatingCityId = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MultiModalConfigs.MultiModalConfigs -> m (Maybe Domain.Types.MultiModalConfigs.MultiModalConfigs))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MultiModalConfigs.MultiModalConfigs -> m ())
updateByPrimaryKey (Domain.Types.MultiModalConfigs.MultiModalConfigs {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.busFilterTimeBufferInSeconds busFilterTimeBufferInSeconds,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.enableBusFiltering enableBusFiltering,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.nearbyDriverSearchRadius nearbyDriverSearchRadius,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MultiModalConfigs Domain.Types.MultiModalConfigs.MultiModalConfigs where
  fromTType' (Beam.MultiModalConfigsT {..}) = do
    pure $
      Just
        Domain.Types.MultiModalConfigs.MultiModalConfigs
          { busFilterTimeBufferInSeconds = busFilterTimeBufferInSeconds,
            createdAt = createdAt,
            enableBusFiltering = enableBusFiltering,
            id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            nearbyDriverSearchRadius = nearbyDriverSearchRadius,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MultiModalConfigs Domain.Types.MultiModalConfigs.MultiModalConfigs where
  toTType' (Domain.Types.MultiModalConfigs.MultiModalConfigs {..}) = do
    Beam.MultiModalConfigsT
      { Beam.busFilterTimeBufferInSeconds = busFilterTimeBufferInSeconds,
        Beam.createdAt = createdAt,
        Beam.enableBusFiltering = enableBusFiltering,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.nearbyDriverSearchRadius = nearbyDriverSearchRadius,
        Beam.updatedAt = updatedAt
      }
