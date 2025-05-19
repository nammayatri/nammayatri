{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSConfig where

import qualified Domain.Types.FRFSConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSConfig.FRFSConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSConfig.FRFSConfig] -> m ())
createMany = traverse_ create

findByBppId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FRFSConfig.FRFSConfig))
findByBppId bppId = do findOneWithKV [Se.Is Beam.bppId $ Se.Eq bppId]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSConfig.FRFSConfig -> m (Maybe Domain.Types.FRFSConfig.FRFSConfig))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateConfigByBppId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
updateConfigByBppId bppId = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.updatedAt _now] [Se.Is Beam.bppId $ Se.Eq bppId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSConfig.FRFSConfig -> m (Maybe Domain.Types.FRFSConfig.FRFSConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSConfig.FRFSConfig -> m ())
updateByPrimaryKey (Domain.Types.FRFSConfig.FRFSConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppId bppId,
      Se.Set Beam.config config,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSConfig Domain.Types.FRFSConfig.FRFSConfig where
  fromTType' (Beam.FRFSConfigT {..}) = do
    pure $
      Just
        Domain.Types.FRFSConfig.FRFSConfig
          { bppId = bppId,
            config = config,
            id = Kernel.Types.Id.Id id,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt
          }

instance ToTType' Beam.FRFSConfig Domain.Types.FRFSConfig.FRFSConfig where
  toTType' (Domain.Types.FRFSConfig.FRFSConfig {..}) = do
    Beam.FRFSConfigT
      { Beam.bppId = bppId,
        Beam.config = config,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt
      }
