{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetMemberAssociation where

import qualified Domain.Types.FleetMemberAssociation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetMemberAssociation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetMemberAssociation.FleetMemberAssociation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetMemberAssociation.FleetMemberAssociation] -> m ())
createMany = traverse_ create

deleteByMemberId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteByMemberId fleetMemberId = do deleteWithKV [Se.Is Beam.fleetMemberId $ Se.Eq fleetMemberId]

findAllMemberByFleetOwnerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.FleetMemberAssociation.FleetMemberAssociation])
findAllMemberByFleetOwnerId fleetOwnerId = do findAllWithKV [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]

findByfleetMemberIdAndFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Bool -> m (Maybe Domain.Types.FleetMemberAssociation.FleetMemberAssociation))
findByfleetMemberIdAndFleetOwnerId fleetMemberId fleetOwnerId enabled = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.fleetMemberId $ Se.Eq fleetMemberId,
          Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId,
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
    ]

updateFleetMemberActiveStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updateFleetMemberActiveStatus enabled fleetMemberId fleetOwnerId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.enabled enabled, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.fleetMemberId $ Se.Eq fleetMemberId, Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FleetMemberAssociation.FleetMemberAssociation))
findByPrimaryKey fleetMemberId = do findOneWithKV [Se.And [Se.Is Beam.fleetMemberId $ Se.Eq fleetMemberId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetMemberAssociation.FleetMemberAssociation -> m ())
updateByPrimaryKey (Domain.Types.FleetMemberAssociation.FleetMemberAssociation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.fleetOwnerId fleetOwnerId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.fleetMemberId $ Se.Eq fleetMemberId]]

instance FromTType' Beam.FleetMemberAssociation Domain.Types.FleetMemberAssociation.FleetMemberAssociation where
  fromTType' (Beam.FleetMemberAssociationT {..}) = do
    pure $
      Just
        Domain.Types.FleetMemberAssociation.FleetMemberAssociation
          { createdAt = createdAt,
            enabled = enabled,
            fleetMemberId = fleetMemberId,
            fleetOwnerId = fleetOwnerId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetMemberAssociation Domain.Types.FleetMemberAssociation.FleetMemberAssociation where
  toTType' (Domain.Types.FleetMemberAssociation.FleetMemberAssociation {..}) = do
    Beam.FleetMemberAssociationT
      { Beam.createdAt = createdAt,
        Beam.enabled = enabled,
        Beam.fleetMemberId = fleetMemberId,
        Beam.fleetOwnerId = fleetOwnerId,
        Beam.updatedAt = updatedAt
      }
