{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.FleetBadgeAssociation where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.FleetBadgeAssociation
import qualified Storage.Beam.FleetBadgeAssociation as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.FleetBadgeAssociation Domain.Types.FleetBadgeAssociation.FleetBadgeAssociation
    where fromTType' (Beam.FleetBadgeAssociationT {..}) = do pure $ Just Domain.Types.FleetBadgeAssociation.FleetBadgeAssociation{associatedOn = associatedOn,
                                                                                                                                  associatedTill = associatedTill,
                                                                                                                                  badgeId = Kernel.Types.Id.Id badgeId,
                                                                                                                                  badgeType = badgeType,
                                                                                                                                  createdAt = createdAt,
                                                                                                                                  driverId = Kernel.Types.Id.Id driverId,
                                                                                                                                  fleetOwnerId = fleetOwnerId,
                                                                                                                                  id = Kernel.Types.Id.Id id,
                                                                                                                                  isActive = isActive,
                                                                                                                                  updatedAt = updatedAt}
instance ToTType' Beam.FleetBadgeAssociation Domain.Types.FleetBadgeAssociation.FleetBadgeAssociation
    where toTType' (Domain.Types.FleetBadgeAssociation.FleetBadgeAssociation {..}) = do Beam.FleetBadgeAssociationT{Beam.associatedOn = associatedOn,
                                                                                                                    Beam.associatedTill = associatedTill,
                                                                                                                    Beam.badgeId = Kernel.Types.Id.getId badgeId,
                                                                                                                    Beam.badgeType = badgeType,
                                                                                                                    Beam.createdAt = createdAt,
                                                                                                                    Beam.driverId = Kernel.Types.Id.getId driverId,
                                                                                                                    Beam.fleetOwnerId = fleetOwnerId,
                                                                                                                    Beam.id = Kernel.Types.Id.getId id,
                                                                                                                    Beam.isActive = isActive,
                                                                                                                    Beam.updatedAt = updatedAt}



