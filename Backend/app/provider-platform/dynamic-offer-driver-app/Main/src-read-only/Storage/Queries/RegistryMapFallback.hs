{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RegistryMapFallback where

import qualified Domain.Types.RegistryMapFallback
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RegistryMapFallback as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RegistryMapFallback.RegistryMapFallback -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RegistryMapFallback.RegistryMapFallback] -> m ())
createMany = traverse_ create

findBySubscriberId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.RegistryMapFallback.RegistryMapFallback])
findBySubscriberId subscriberId = do findAllWithKV [Se.Is Beam.subscriberId $ Se.Eq subscriberId]

findBySubscriberIdAndUniqueId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.RegistryMapFallback.RegistryMapFallback))
findBySubscriberIdAndUniqueId subscriberId uniqueId = do findOneWithKV [Se.And [Se.Is Beam.subscriberId $ Se.Eq subscriberId, Se.Is Beam.uniqueId $ Se.Eq uniqueId]]

findByUniqueId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.RegistryMapFallback.RegistryMapFallback])
findByUniqueId uniqueId = do findAllWithKV [Se.Is Beam.uniqueId $ Se.Eq uniqueId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.RegistryMapFallback.RegistryMapFallback))
findByPrimaryKey subscriberId uniqueId = do findOneWithKV [Se.And [Se.Is Beam.subscriberId $ Se.Eq subscriberId, Se.Is Beam.uniqueId $ Se.Eq uniqueId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RegistryMapFallback.RegistryMapFallback -> m ())
updateByPrimaryKey (Domain.Types.RegistryMapFallback.RegistryMapFallback {..}) = do
  updateWithKV
    [Se.Set Beam.registryUrl (Kernel.Prelude.showBaseUrl registryUrl)]
    [ Se.And
        [ Se.Is Beam.subscriberId $ Se.Eq subscriberId,
          Se.Is Beam.uniqueId $ Se.Eq uniqueId
        ]
    ]

instance FromTType' Beam.RegistryMapFallback Domain.Types.RegistryMapFallback.RegistryMapFallback where
  fromTType' (Beam.RegistryMapFallbackT {..}) = do
    registryUrl' <- Kernel.Prelude.parseBaseUrl registryUrl
    pure $ Just Domain.Types.RegistryMapFallback.RegistryMapFallback {registryUrl = registryUrl', subscriberId = subscriberId, uniqueId = uniqueId}

instance ToTType' Beam.RegistryMapFallback Domain.Types.RegistryMapFallback.RegistryMapFallback where
  toTType' (Domain.Types.RegistryMapFallback.RegistryMapFallback {..}) = do
    Beam.RegistryMapFallbackT
      { Beam.registryUrl = Kernel.Prelude.showBaseUrl registryUrl,
        Beam.subscriberId = subscriberId,
        Beam.uniqueId = uniqueId
      }
