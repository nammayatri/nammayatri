{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.FleetMemberAssociation where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.FleetMemberAssociation
import qualified Storage.Beam.FleetMemberAssociation as Beam



instance FromTType' Beam.FleetMemberAssociation Domain.Types.FleetMemberAssociation.FleetMemberAssociation
    where fromTType' (Beam.FleetMemberAssociationT {..}) = do pure $ Just Domain.Types.FleetMemberAssociation.FleetMemberAssociation{createdAt = createdAt,
                                                                                                                                     enabled = enabled,
                                                                                                                                     fleetMemberId = fleetMemberId,
                                                                                                                                     fleetOwnerId = fleetOwnerId,
                                                                                                                                     groupCode = groupCode,
                                                                                                                                     isFleetOwner = isFleetOwner,
                                                                                                                                     level = level,
                                                                                                                                     order = order,
                                                                                                                                     parentGroupCode = parentGroupCode,
                                                                                                                                     updatedAt = updatedAt}
instance ToTType' Beam.FleetMemberAssociation Domain.Types.FleetMemberAssociation.FleetMemberAssociation
    where toTType' (Domain.Types.FleetMemberAssociation.FleetMemberAssociation {..}) = do Beam.FleetMemberAssociationT{Beam.createdAt = createdAt,
                                                                                                                       Beam.enabled = enabled,
                                                                                                                       Beam.fleetMemberId = fleetMemberId,
                                                                                                                       Beam.fleetOwnerId = fleetOwnerId,
                                                                                                                       Beam.groupCode = groupCode,
                                                                                                                       Beam.isFleetOwner = isFleetOwner,
                                                                                                                       Beam.level = level,
                                                                                                                       Beam.order = order,
                                                                                                                       Beam.parentGroupCode = parentGroupCode,
                                                                                                                       Beam.updatedAt = updatedAt}



