{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetControlGroup where

import qualified Domain.Types.FleetControlGroup
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetControlGroup as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetControlGroup.FleetControlGroup -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetControlGroup.FleetControlGroup] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FleetControlGroup.FleetControlGroup -> m (Maybe Domain.Types.FleetControlGroup.FleetControlGroup))
findByPrimaryKey fleetControlGroupId = do findOneWithKV [Se.And [Se.Is Beam.fleetControlGroupId $ Se.Eq (Kernel.Types.Id.getId fleetControlGroupId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetControlGroup.FleetControlGroup -> m ())
updateByPrimaryKey (Domain.Types.FleetControlGroup.FleetControlGroup {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.fleetControlGroupName fleetControlGroupName,
      Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId fleetOwnerId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.fleetControlGroupId $ Se.Eq (Kernel.Types.Id.getId fleetControlGroupId)]]

instance FromTType' Beam.FleetControlGroup Domain.Types.FleetControlGroup.FleetControlGroup where
  fromTType' (Beam.FleetControlGroupT {..}) = do
    pure $
      Just
        Domain.Types.FleetControlGroup.FleetControlGroup
          { fleetControlGroupId = Kernel.Types.Id.Id fleetControlGroupId,
            fleetControlGroupName = fleetControlGroupName,
            fleetOwnerId = Kernel.Types.Id.Id fleetOwnerId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetControlGroup Domain.Types.FleetControlGroup.FleetControlGroup where
  toTType' (Domain.Types.FleetControlGroup.FleetControlGroup {..}) = do
    Beam.FleetControlGroupT
      { Beam.fleetControlGroupId = Kernel.Types.Id.getId fleetControlGroupId,
        Beam.fleetControlGroupName = fleetControlGroupName,
        Beam.fleetOwnerId = Kernel.Types.Id.getId fleetOwnerId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
