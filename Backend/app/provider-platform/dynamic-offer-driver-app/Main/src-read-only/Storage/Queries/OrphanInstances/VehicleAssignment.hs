{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.VehicleAssignment where

import qualified Domain.Types.VehicleAssignment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.VehicleAssignment as Beam

instance FromTType' Beam.VehicleAssignment Domain.Types.VehicleAssignment.VehicleAssignment where
  fromTType' (Beam.VehicleAssignmentT {..}) = do
    pure $
      Just
        Domain.Types.VehicleAssignment.VehicleAssignment
          { amount = amount,
            assignedAt = assignedAt,
            assignmentStatus = assignmentStatus,
            createdAt = createdAt,
            fleetOwnerId = Kernel.Types.Id.Id fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            placeId = placeId,
            ticketId = ticketId,
            updatedAt = updatedAt,
            vehicleId = Kernel.Types.Id.Id vehicleId
          }

instance ToTType' Beam.VehicleAssignment Domain.Types.VehicleAssignment.VehicleAssignment where
  toTType' (Domain.Types.VehicleAssignment.VehicleAssignment {..}) = do
    Beam.VehicleAssignmentT
      { Beam.amount = amount,
        Beam.assignedAt = assignedAt,
        Beam.assignmentStatus = assignmentStatus,
        Beam.createdAt = createdAt,
        Beam.fleetOwnerId = Kernel.Types.Id.getId fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.placeId = placeId,
        Beam.ticketId = ticketId,
        Beam.updatedAt = updatedAt,
        Beam.vehicleId = Kernel.Types.Id.getId vehicleId
      }
