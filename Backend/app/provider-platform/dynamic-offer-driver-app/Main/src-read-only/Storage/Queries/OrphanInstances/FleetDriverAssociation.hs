{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FleetDriverAssociation where

import qualified Domain.Types.FleetDriverAssociation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FleetDriverAssociation as Beam

instance FromTType' Beam.FleetDriverAssociation Domain.Types.FleetDriverAssociation.FleetDriverAssociation where
  fromTType' (Beam.FleetDriverAssociationT {..}) = do
    pure $
      Just
        Domain.Types.FleetDriverAssociation.FleetDriverAssociation
          { associatedOn = associatedOn,
            associatedTill = associatedTill,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            enableCashRide = enableCashRide,
            fleetOwnerId = fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            isActive = isActive,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            onboardedOperatorId = Kernel.Types.Id.Id <$> onboardedOperatorId,
            onboardingVehicleCategory = onboardingVehicleCategory,
            requestReason = requestReason,
            responseReason = responseReason,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetDriverAssociation Domain.Types.FleetDriverAssociation.FleetDriverAssociation where
  toTType' (Domain.Types.FleetDriverAssociation.FleetDriverAssociation {..}) = do
    Beam.FleetDriverAssociationT
      { Beam.associatedOn = associatedOn,
        Beam.associatedTill = associatedTill,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.enableCashRide = enableCashRide,
        Beam.fleetOwnerId = fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isActive = isActive,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.onboardedOperatorId = Kernel.Types.Id.getId <$> onboardedOperatorId,
        Beam.onboardingVehicleCategory = onboardingVehicleCategory,
        Beam.requestReason = requestReason,
        Beam.responseReason = responseReason,
        Beam.updatedAt = updatedAt
      }
