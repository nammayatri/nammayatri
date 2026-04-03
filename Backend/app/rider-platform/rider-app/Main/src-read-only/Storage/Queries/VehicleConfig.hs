{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.VehicleConfig where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.VehicleConfig
import qualified Storage.Beam.VehicleConfig as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.BecknConfig
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleConfig.VehicleConfig -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleConfig.VehicleConfig] -> m ())
createMany = traverse_ create
findAllByBecknConfigId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig -> m ([Domain.Types.VehicleConfig.VehicleConfig]))
findAllByBecknConfigId becknConfigId = do findAllWithKV [Se.Is Beam.becknConfigId $ Se.Eq (Kernel.Types.Id.getId becknConfigId)]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VehicleConfig.VehicleConfig -> m (Maybe Domain.Types.VehicleConfig.VehicleConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleConfig.VehicleConfig -> m ())
updateByPrimaryKey (Domain.Types.VehicleConfig.VehicleConfig {..}) = do {_now <- getCurrentTime;
                                                                         updateWithKV [Se.Set Beam.becknConfigId (Kernel.Types.Id.getId becknConfigId),
                                                                                       Se.Set Beam.blackListedSubscribers blackListedSubscribers,
                                                                                       Se.Set Beam.buyerFinderFee buyerFinderFee,
                                                                                       Se.Set Beam.category category,
                                                                                       Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                                       Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
                                                                                       Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



instance FromTType' Beam.VehicleConfig Domain.Types.VehicleConfig.VehicleConfig
    where fromTType' (Beam.VehicleConfigT {..}) = do pure $ Just Domain.Types.VehicleConfig.VehicleConfig{becknConfigId = Kernel.Types.Id.Id becknConfigId,
                                                                                                          blackListedSubscribers = blackListedSubscribers,
                                                                                                          buyerFinderFee = buyerFinderFee,
                                                                                                          category = category,
                                                                                                          id = Kernel.Types.Id.Id id,
                                                                                                          merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                          merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
                                                                                                          createdAt = createdAt,
                                                                                                          updatedAt = updatedAt}
instance ToTType' Beam.VehicleConfig Domain.Types.VehicleConfig.VehicleConfig
    where toTType' (Domain.Types.VehicleConfig.VehicleConfig {..}) = do Beam.VehicleConfigT{Beam.becknConfigId = Kernel.Types.Id.getId becknConfigId,
                                                                                            Beam.blackListedSubscribers = blackListedSubscribers,
                                                                                            Beam.buyerFinderFee = buyerFinderFee,
                                                                                            Beam.category = category,
                                                                                            Beam.id = Kernel.Types.Id.getId id,
                                                                                            Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                            Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
                                                                                            Beam.createdAt = createdAt,
                                                                                            Beam.updatedAt = updatedAt}



