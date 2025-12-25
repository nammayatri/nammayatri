{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetMemberAssociation (module Storage.Queries.FleetMemberAssociation, module ReExport) where

import qualified Domain.Types.FleetMemberAssociation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetMemberAssociation as Beam
import Storage.Queries.FleetMemberAssociationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetMemberAssociation.FleetMemberAssociation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetMemberAssociation.FleetMemberAssociation] -> m ())
createMany = traverse_ create

findAllActiveByfleetMemberId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Bool -> m [Domain.Types.FleetMemberAssociation.FleetMemberAssociation])
findAllActiveByfleetMemberId fleetMemberId enabled = do findAllWithKV [Se.And [Se.Is Beam.fleetMemberId $ Se.Eq fleetMemberId, Se.Is Beam.enabled $ Se.Eq enabled]]

findAllByfleetMemberId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.FleetMemberAssociation.FleetMemberAssociation])
findAllByfleetMemberId fleetMemberId = do findAllWithKV [Se.And [Se.Is Beam.fleetMemberId $ Se.Eq fleetMemberId]]

findAllWithOwnerIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Prelude.Text] -> m [Domain.Types.FleetMemberAssociation.FleetMemberAssociation])
findAllWithOwnerIds fleetOwnerId = do findAllWithKV [Se.And [Se.Is Beam.fleetOwnerId $ Se.In fleetOwnerId]]

updateFleetMemberActiveStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updateFleetMemberActiveStatus enabled fleetMemberId fleetOwnerId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.enabled enabled, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.fleetMemberId $ Se.Eq fleetMemberId, Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]]

updateFleetMembersActiveStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Prelude.Text -> [Kernel.Prelude.Text] -> m ())
updateFleetMembersActiveStatus enabled fleetMemberId fleetOwnerId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.enabled enabled, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.fleetMemberId $ Se.Eq fleetMemberId, Se.Is Beam.fleetOwnerId $ Se.In fleetOwnerId]]

updateFleetMembersActiveStatusByGroupCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ())
updateFleetMembersActiveStatusByGroupCode enabled fleetMemberId groupCode = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.enabled enabled, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.fleetMemberId $ Se.Eq fleetMemberId, Se.Is Beam.groupCode $ Se.Eq groupCode]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.FleetMemberAssociation.FleetMemberAssociation))
findByPrimaryKey fleetMemberId fleetOwnerId = do findOneWithKV [Se.And [Se.Is Beam.fleetMemberId $ Se.Eq fleetMemberId, Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetMemberAssociation.FleetMemberAssociation -> m ())
updateByPrimaryKey (Domain.Types.FleetMemberAssociation.FleetMemberAssociation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.enabled enabled,
      Se.Set Beam.groupCode groupCode,
      Se.Set Beam.isFleetOwner isFleetOwner,
      Se.Set Beam.level level,
      Se.Set Beam.order order,
      Se.Set Beam.parentGroupCode parentGroupCode,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.fleetMemberId $ Se.Eq fleetMemberId, Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]]
