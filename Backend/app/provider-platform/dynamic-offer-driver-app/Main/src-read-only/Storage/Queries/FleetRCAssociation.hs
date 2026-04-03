{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.FleetRCAssociation (module Storage.Queries.FleetRCAssociation, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.FleetRCAssociationExtra as ReExport
import qualified Domain.Types.FleetRCAssociation
import qualified Storage.Beam.FleetRCAssociation as Beam
import qualified Kernel.Types.Id
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetRCAssociation.FleetRCAssociation -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetRCAssociation.FleetRCAssociation] -> m ())
createMany = traverse_ create
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FleetRCAssociation.FleetRCAssociation -> m (Maybe Domain.Types.FleetRCAssociation.FleetRCAssociation))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.FleetRCAssociation.FleetRCAssociation -> m (Maybe Domain.Types.FleetRCAssociation.FleetRCAssociation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetRCAssociation.FleetRCAssociation -> m ())
updateByPrimaryKey (Domain.Types.FleetRCAssociation.FleetRCAssociation {..}) = do {_now <- getCurrentTime;
                                                                                   updateWithKV [Se.Set Beam.associatedOn associatedOn,
                                                                                                 Se.Set Beam.associatedTill associatedTill,
                                                                                                 Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId fleetOwnerId),
                                                                                                 Se.Set Beam.rcId (Kernel.Types.Id.getId rcId),
                                                                                                 Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                                                 Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
                                                                                                 Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



