{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.FleetOperatorAssociation (module Storage.Queries.FleetOperatorAssociation, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.FleetOperatorAssociationExtra as ReExport
import qualified Domain.Types.FleetOperatorAssociation
import qualified Storage.Beam.FleetOperatorAssociation as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation] -> m ())
createMany = traverse_ create
deleteByFleetOwnerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteByFleetOwnerId fleetOwnerId = do deleteWithKV [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]
findAllFleetAssociations :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation]))
findAllFleetAssociations fleetOwnerId = do findAllWithKV [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]
findByFleetIdAndOperatorId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                              (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Bool -> m (Maybe Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation))
findByFleetIdAndOperatorId fleetOwnerId operatorId isActive = do findOneWithKV [Se.And [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId,
                                                                                        Se.Is Beam.operatorId $ Se.Eq operatorId,
                                                                                        Se.Is Beam.isActive $ Se.Eq isActive]]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation -> m (Maybe Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation -> m ())
updateByPrimaryKey (Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation {..}) = do {_now <- getCurrentTime;
                                                                                               updateWithKV [Se.Set Beam.associatedOn associatedOn,
                                                                                                             Se.Set Beam.associatedTill associatedTill,
                                                                                                             Se.Set Beam.fleetOwnerId fleetOwnerId,
                                                                                                             Se.Set Beam.isActive isActive,
                                                                                                             Se.Set Beam.operatorId operatorId,
                                                                                                             Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                                                             Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
                                                                                                             Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



