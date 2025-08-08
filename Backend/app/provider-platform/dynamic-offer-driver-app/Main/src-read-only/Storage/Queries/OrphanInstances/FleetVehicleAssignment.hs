{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FleetVehicleAssignment where

import qualified Domain.Types.FleetVehicleAssignment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FleetVehicleAssignment as Beam

instance FromTType' Beam.FleetVehicleAssignment Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment where
  fromTType' (Beam.FleetVehicleAssignmentT {..}) = do
    pure $
      Just
        Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment
          { amount = amount,
            assignedAt = assignedAt,
            assignmentStatus = assignmentStatus,
            createdAt = createdAt,
            fleetOwnerId = Kernel.Types.Id.Id <$> fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            ticketBookingId = ticketBookingId,
            ticketBookingServiceId = ticketBookingServiceId,
            ticketPlaceId = ticketPlaceId,
            vehicleId = Kernel.Types.Id.Id <$> vehicleId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetVehicleAssignment Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment where
  toTType' (Domain.Types.FleetVehicleAssignment.FleetVehicleAssignment {..}) = do
    Beam.FleetVehicleAssignmentT
      { Beam.amount = amount,
        Beam.assignedAt = assignedAt,
        Beam.assignmentStatus = assignmentStatus,
        Beam.createdAt = createdAt,
        Beam.fleetOwnerId = Kernel.Types.Id.getId <$> fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.ticketBookingId = ticketBookingId,
        Beam.ticketBookingServiceId = ticketBookingServiceId,
        Beam.ticketPlaceId = ticketPlaceId,
        Beam.vehicleId = Kernel.Types.Id.getId <$> vehicleId,
        Beam.updatedAt = updatedAt
      }
