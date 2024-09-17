{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MultiModalNetwork where

import qualified Domain.Types.MultiModalNetwork
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MultiModalNetwork as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MultiModalNetwork.MultiModalNetwork -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MultiModalNetwork.MultiModalNetwork] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MultiModalNetwork.MultiModalNetwork -> m (Maybe Domain.Types.MultiModalNetwork.MultiModalNetwork))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByNetworkType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MultiModalNetwork.MultiModalNetworkType -> m ([Domain.Types.MultiModalNetwork.MultiModalNetwork]))
findByNetworkType networkType = do findAllWithKV [Se.Is Beam.networkType $ Se.Eq networkType]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MultiModalNetwork.MultiModalNetwork -> m (Maybe Domain.Types.MultiModalNetwork.MultiModalNetwork))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MultiModalNetwork.MultiModalNetwork -> m ())
updateByPrimaryKey (Domain.Types.MultiModalNetwork.MultiModalNetwork {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.name name,
      Se.Set Beam.networkClass networkClass,
      Se.Set Beam.networkCode networkCode,
      Se.Set Beam.networkType networkType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MultiModalNetwork Domain.Types.MultiModalNetwork.MultiModalNetwork where
  fromTType' (Beam.MultiModalNetworkT {..}) = do
    pure $
      Just
        Domain.Types.MultiModalNetwork.MultiModalNetwork
          { id = Kernel.Types.Id.Id id,
            name = name,
            networkClass = networkClass,
            networkCode = networkCode,
            networkType = networkType,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MultiModalNetwork Domain.Types.MultiModalNetwork.MultiModalNetwork where
  toTType' (Domain.Types.MultiModalNetwork.MultiModalNetwork {..}) = do
    Beam.MultiModalNetworkT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.networkClass = networkClass,
        Beam.networkCode = networkCode,
        Beam.networkType = networkType,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
