{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.RegistryMapFallback where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.RegistryMapFallback
import qualified Storage.Beam.RegistryMapFallback as Beam
import qualified Kernel.Prelude
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RegistryMapFallback.RegistryMapFallback -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RegistryMapFallback.RegistryMapFallback] -> m ())
createMany = traverse_ create
findBySubscriberId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.RegistryMapFallback.RegistryMapFallback]))
findBySubscriberId subscriberId = do findAllWithKV [Se.Is Beam.subscriberId $ Se.Eq subscriberId]
findBySubscriberIdAndUniqueId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.RegistryMapFallback.RegistryMapFallback))
findBySubscriberIdAndUniqueId subscriberId uniqueId = do findOneWithKV [Se.And [Se.Is Beam.subscriberId $ Se.Eq subscriberId, Se.Is Beam.uniqueId $ Se.Eq uniqueId]]
findByUniqueId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.RegistryMapFallback.RegistryMapFallback]))
findByUniqueId uniqueId = do findAllWithKV [Se.Is Beam.uniqueId $ Se.Eq uniqueId]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.RegistryMapFallback.RegistryMapFallback))
findByPrimaryKey subscriberId uniqueId = do findOneWithKV [Se.And [Se.Is Beam.subscriberId $ Se.Eq subscriberId, Se.Is Beam.uniqueId $ Se.Eq uniqueId]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RegistryMapFallback.RegistryMapFallback -> m ())
updateByPrimaryKey (Domain.Types.RegistryMapFallback.RegistryMapFallback {..}) = do updateWithKV [Se.Set Beam.registryUrl (Kernel.Prelude.showBaseUrl registryUrl)] [Se.And [Se.Is Beam.subscriberId $ Se.Eq subscriberId,
                                                                                                                                                                             Se.Is Beam.uniqueId $ Se.Eq uniqueId]]



instance FromTType' Beam.RegistryMapFallback Domain.Types.RegistryMapFallback.RegistryMapFallback
    where fromTType' (Beam.RegistryMapFallbackT {..}) = do {registryUrl' <- Kernel.Prelude.parseBaseUrl registryUrl;
                                                            pure $ Just Domain.Types.RegistryMapFallback.RegistryMapFallback{registryUrl = registryUrl', subscriberId = subscriberId, uniqueId = uniqueId}}
instance ToTType' Beam.RegistryMapFallback Domain.Types.RegistryMapFallback.RegistryMapFallback
    where toTType' (Domain.Types.RegistryMapFallback.RegistryMapFallback {..}) = do Beam.RegistryMapFallbackT{Beam.registryUrl = Kernel.Prelude.showBaseUrl registryUrl,
                                                                                                              Beam.subscriberId = subscriberId,
                                                                                                              Beam.uniqueId = uniqueId}



