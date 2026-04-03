{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.FleetDriverAssociation where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.FleetDriverAssociation
import qualified Storage.Beam.FleetDriverAssociation as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.FleetDriverAssociation Domain.Types.FleetDriverAssociation.FleetDriverAssociation
    where fromTType' (Beam.FleetDriverAssociationT {..}) = do pure $ Just Domain.Types.FleetDriverAssociation.FleetDriverAssociation{associatedOn = associatedOn,
                                                                                                                                     associatedTill = associatedTill,
                                                                                                                                     createdAt = createdAt,
                                                                                                                                     driverId = Kernel.Types.Id.Id driverId,
                                                                                                                                     fleetOwnerId = fleetOwnerId,
                                                                                                                                     id = Kernel.Types.Id.Id id,
                                                                                                                                     isActive = isActive,
                                                                                                                                     onboardedOperatorId = Kernel.Types.Id.Id <$> onboardedOperatorId,
                                                                                                                                     onboardingVehicleCategory = onboardingVehicleCategory,
                                                                                                                                     requestReason = requestReason,
                                                                                                                                     responseReason = responseReason,
                                                                                                                                     updatedAt = updatedAt}
instance ToTType' Beam.FleetDriverAssociation Domain.Types.FleetDriverAssociation.FleetDriverAssociation
    where toTType' (Domain.Types.FleetDriverAssociation.FleetDriverAssociation {..}) = do Beam.FleetDriverAssociationT{Beam.associatedOn = associatedOn,
                                                                                                                       Beam.associatedTill = associatedTill,
                                                                                                                       Beam.createdAt = createdAt,
                                                                                                                       Beam.driverId = Kernel.Types.Id.getId driverId,
                                                                                                                       Beam.fleetOwnerId = fleetOwnerId,
                                                                                                                       Beam.id = Kernel.Types.Id.getId id,
                                                                                                                       Beam.isActive = isActive,
                                                                                                                       Beam.onboardedOperatorId = Kernel.Types.Id.getId <$> onboardedOperatorId,
                                                                                                                       Beam.onboardingVehicleCategory = onboardingVehicleCategory,
                                                                                                                       Beam.requestReason = requestReason,
                                                                                                                       Beam.responseReason = responseReason,
                                                                                                                       Beam.updatedAt = updatedAt}



