{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.CancellationCharges where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.CancellationCharges
import qualified Storage.Beam.CancellationCharges as Beam
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Prelude
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationCharges.CancellationCharges -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CancellationCharges.CancellationCharges] -> m ())
createMany = traverse_ create
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.CancellationCharges.CancellationCharges -> m (Maybe Domain.Types.CancellationCharges.CancellationCharges))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationCharges.CancellationCharges -> m ())
updateByPrimaryKey (Domain.Types.CancellationCharges.CancellationCharges {..}) = do updateWithKV [Se.Set Beam.cancellationCharges cancellationCharges,
                                                                                                  Se.Set Beam.currency (Kernel.Prelude.Just currency),
                                                                                                  Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
                                                                                                  Se.Set Beam.rideId (Kernel.Types.Id.getId <$> rideId)] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]



instance FromTType' Beam.CancellationCharges Domain.Types.CancellationCharges.CancellationCharges
    where fromTType' (Beam.CancellationChargesT {..}) = do pure $ Just Domain.Types.CancellationCharges.CancellationCharges{cancellationCharges = cancellationCharges,
                                                                                                                            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
                                                                                                                            driverId = Kernel.Types.Id.Id driverId,
                                                                                                                            id = Kernel.Types.Id.Id id,
                                                                                                                            rideId = Kernel.Types.Id.Id <$> rideId}
instance ToTType' Beam.CancellationCharges Domain.Types.CancellationCharges.CancellationCharges
    where toTType' (Domain.Types.CancellationCharges.CancellationCharges {..}) = do Beam.CancellationChargesT{Beam.cancellationCharges = cancellationCharges,
                                                                                                              Beam.currency = Kernel.Prelude.Just currency,
                                                                                                              Beam.driverId = Kernel.Types.Id.getId driverId,
                                                                                                              Beam.id = Kernel.Types.Id.getId id,
                                                                                                              Beam.rideId = Kernel.Types.Id.getId <$> rideId}



