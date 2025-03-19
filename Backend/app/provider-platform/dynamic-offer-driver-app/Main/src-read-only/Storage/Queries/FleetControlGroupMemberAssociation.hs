{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetControlGroupMemberAssociation where

import qualified Domain.Types.FleetControlGroup
import qualified Domain.Types.FleetControlGroupMemberAssociation
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetControlGroupMemberAssociation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetControlGroupMemberAssociation.FleetControlGroupMemberAssociation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetControlGroupMemberAssociation.FleetControlGroupMemberAssociation] -> m ())
createMany = traverse_ create

findAllByControlGroupId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FleetControlGroup.FleetControlGroup -> m ([Domain.Types.FleetControlGroupMemberAssociation.FleetControlGroupMemberAssociation]))
findAllByControlGroupId fleetControlGroupId = do findAllWithKV [Se.Is Beam.fleetControlGroupId $ Se.Eq (Kernel.Types.Id.getId fleetControlGroupId)]

findAllControlGroupsByFleetMemberId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.FleetControlGroupMemberAssociation.FleetControlGroupMemberAssociation]))
findAllControlGroupsByFleetMemberId fleetMemberId = do findAllWithKV [Se.Is Beam.fleetMemberId $ Se.Eq (Kernel.Types.Id.getId fleetMemberId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FleetControlGroup.FleetControlGroup -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.FleetControlGroupMemberAssociation.FleetControlGroupMemberAssociation))
findByPrimaryKey fleetControlGroupId fleetMemberId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.fleetControlGroupId $ Se.Eq (Kernel.Types.Id.getId fleetControlGroupId),
          Se.Is Beam.fleetMemberId $ Se.Eq (Kernel.Types.Id.getId fleetMemberId)
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetControlGroupMemberAssociation.FleetControlGroupMemberAssociation -> m ())
updateByPrimaryKey (Domain.Types.FleetControlGroupMemberAssociation.FleetControlGroupMemberAssociation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.fleetControlGroupId $ Se.Eq (Kernel.Types.Id.getId fleetControlGroupId),
          Se.Is Beam.fleetMemberId $ Se.Eq (Kernel.Types.Id.getId fleetMemberId)
        ]
    ]

instance FromTType' Beam.FleetControlGroupMemberAssociation Domain.Types.FleetControlGroupMemberAssociation.FleetControlGroupMemberAssociation where
  fromTType' (Beam.FleetControlGroupMemberAssociationT {..}) = do
    pure $
      Just
        Domain.Types.FleetControlGroupMemberAssociation.FleetControlGroupMemberAssociation
          { fleetControlGroupId = Kernel.Types.Id.Id fleetControlGroupId,
            fleetMemberId = Kernel.Types.Id.Id fleetMemberId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetControlGroupMemberAssociation Domain.Types.FleetControlGroupMemberAssociation.FleetControlGroupMemberAssociation where
  toTType' (Domain.Types.FleetControlGroupMemberAssociation.FleetControlGroupMemberAssociation {..}) = do
    Beam.FleetControlGroupMemberAssociationT
      { Beam.fleetControlGroupId = Kernel.Types.Id.getId fleetControlGroupId,
        Beam.fleetMemberId = Kernel.Types.Id.getId fleetMemberId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
