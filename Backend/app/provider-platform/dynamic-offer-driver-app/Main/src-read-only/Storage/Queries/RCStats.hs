{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.RCStats where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.RCStats
import qualified Storage.Beam.RCStats as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.VehicleRegistrationCertificate
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RCStats.RCStats -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RCStats.RCStats] -> m ())
createMany = traverse_ create
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m (Maybe Domain.Types.RCStats.RCStats))
findById rcId = do findOneWithKV [Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId)]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m (Maybe Domain.Types.RCStats.RCStats))
findByPrimaryKey rcId = do findOneWithKV [Se.And [Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RCStats.RCStats -> m ())
updateByPrimaryKey (Domain.Types.RCStats.RCStats {..}) = do {_now <- getCurrentTime;
                                                             updateWithKV [Se.Set Beam.totalRides totalRides,
                                                                           Se.Set Beam.updatedAt _now,
                                                                           Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                           Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)] [Se.And [Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId)]]}



instance FromTType' Beam.RCStats Domain.Types.RCStats.RCStats
    where fromTType' (Beam.RCStatsT {..}) = do pure $ Just Domain.Types.RCStats.RCStats{rcId = Kernel.Types.Id.Id rcId,
                                                                                        totalRides = totalRides,
                                                                                        updatedAt = updatedAt,
                                                                                        merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                        merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
                                                                                        createdAt = createdAt}
instance ToTType' Beam.RCStats Domain.Types.RCStats.RCStats
    where toTType' (Domain.Types.RCStats.RCStats {..}) = do Beam.RCStatsT{Beam.rcId = Kernel.Types.Id.getId rcId,
                                                                          Beam.totalRides = totalRides,
                                                                          Beam.updatedAt = updatedAt,
                                                                          Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                          Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
                                                                          Beam.createdAt = createdAt}



