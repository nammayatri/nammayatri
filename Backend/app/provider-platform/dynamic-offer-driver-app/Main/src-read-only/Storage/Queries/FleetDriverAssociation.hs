{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.FleetDriverAssociation (module Storage.Queries.FleetDriverAssociation, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.FleetDriverAssociationExtra as ReExport
import qualified Domain.Types.FleetDriverAssociation
import qualified Storage.Beam.FleetDriverAssociation as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetDriverAssociation.FleetDriverAssociation -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetDriverAssociation.FleetDriverAssociation] -> m ())
createMany = traverse_ create
deleteByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByDriverId driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
deleteByFleetOwnerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteByFleetOwnerId fleetOwnerId = do deleteWithKV [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.FleetDriverAssociation.FleetDriverAssociation -> m (Maybe Domain.Types.FleetDriverAssociation.FleetDriverAssociation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetDriverAssociation.FleetDriverAssociation -> m ())
updateByPrimaryKey (Domain.Types.FleetDriverAssociation.FleetDriverAssociation {..}) = do {_now <- getCurrentTime;
                                                                                           updateWithKV [Se.Set Beam.associatedOn associatedOn,
                                                                                                         Se.Set Beam.associatedTill associatedTill,
                                                                                                         Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
                                                                                                         Se.Set Beam.fleetOwnerId fleetOwnerId,
                                                                                                         Se.Set Beam.isActive isActive,
                                                                                                         Se.Set Beam.onboardedOperatorId (Kernel.Types.Id.getId <$> onboardedOperatorId),
                                                                                                         Se.Set Beam.onboardingVehicleCategory onboardingVehicleCategory,
                                                                                                         Se.Set Beam.requestReason requestReason,
                                                                                                         Se.Set Beam.responseReason responseReason,
                                                                                                         Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



