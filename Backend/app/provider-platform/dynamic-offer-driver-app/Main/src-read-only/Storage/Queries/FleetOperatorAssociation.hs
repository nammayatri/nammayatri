{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetOperatorAssociation (module Storage.Queries.FleetOperatorAssociation, module ReExport) where

import qualified Domain.Types.FleetOperatorAssociation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOperatorAssociation as Beam
import Storage.Queries.FleetOperatorAssociationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation] -> m ())
createMany = traverse_ create

deleteByOperatorId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteByOperatorId operatorId = do deleteWithKV [Se.Is Beam.operatorId $ Se.Eq operatorId]

findAllOperatorByFleetOwnerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation]))
findAllOperatorByFleetOwnerId fleetOwnerId = do findAllWithKV [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation -> m (Maybe Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation -> m ())
updateByPrimaryKey (Domain.Types.FleetOperatorAssociation.FleetOperatorAssociation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.associatedOn associatedOn,
      Se.Set Beam.associatedTill associatedTill,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.fleetOwnerId fleetOwnerId,
      Se.Set Beam.isActive isActive,
      Se.Set Beam.operatorId operatorId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
