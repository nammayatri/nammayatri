{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FleetMemberAssociation where

import qualified Domain.Types.FleetMemberAssociation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FleetMemberAssociation as Beam

instance FromTType' Beam.FleetMemberAssociation Domain.Types.FleetMemberAssociation.FleetMemberAssociation where
  fromTType' (Beam.FleetMemberAssociationT {..}) = do
    pure $
      Just
        Domain.Types.FleetMemberAssociation.FleetMemberAssociation
          { createdAt = createdAt,
            enabled = enabled,
            fleetMemberId = fleetMemberId,
            fleetOwnerId = fleetOwnerId,
            groupCode = groupCode,
            isFleetOwner = isFleetOwner,
            level = level,
            order = order,
            parentGroupCode = parentGroupCode,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetMemberAssociation Domain.Types.FleetMemberAssociation.FleetMemberAssociation where
  toTType' (Domain.Types.FleetMemberAssociation.FleetMemberAssociation {..}) = do
    Beam.FleetMemberAssociationT
      { Beam.createdAt = createdAt,
        Beam.enabled = enabled,
        Beam.fleetMemberId = fleetMemberId,
        Beam.fleetOwnerId = fleetOwnerId,
        Beam.groupCode = groupCode,
        Beam.isFleetOwner = isFleetOwner,
        Beam.level = level,
        Beam.order = order,
        Beam.parentGroupCode = parentGroupCode,
        Beam.updatedAt = updatedAt
      }
