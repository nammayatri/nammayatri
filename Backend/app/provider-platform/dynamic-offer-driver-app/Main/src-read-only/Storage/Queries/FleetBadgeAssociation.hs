{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetBadgeAssociation (module Storage.Queries.FleetBadgeAssociation, module ReExport) where

import qualified Domain.Types.FleetBadge
import qualified Domain.Types.FleetBadgeAssociation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetBadgeAssociation as Beam
import Storage.Queries.FleetBadgeAssociationExtra as ReExport

deleteByBadgeId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FleetBadge.FleetBadge -> m ())
deleteByBadgeId badgeId = do deleteWithKV [Se.Is Beam.badgeId $ Se.Eq (Kernel.Types.Id.getId badgeId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FleetBadgeAssociation.FleetBadgeAssociation -> m (Maybe Domain.Types.FleetBadgeAssociation.FleetBadgeAssociation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetBadgeAssociation.FleetBadgeAssociation -> m ())
updateByPrimaryKey (Domain.Types.FleetBadgeAssociation.FleetBadgeAssociation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.associatedOn associatedOn,
      Se.Set Beam.associatedTill associatedTill,
      Se.Set Beam.badgeId (Kernel.Types.Id.getId badgeId),
      Se.Set Beam.badgeType badgeType,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.fleetOwnerId fleetOwnerId,
      Se.Set Beam.isActive isActive,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
