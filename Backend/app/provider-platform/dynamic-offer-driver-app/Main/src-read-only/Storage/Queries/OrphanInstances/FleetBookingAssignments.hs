{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FleetBookingAssignments where

import qualified Domain.Types.FleetBookingAssignments
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FleetBookingAssignments as Beam

instance FromTType' Beam.FleetBookingAssignments Domain.Types.FleetBookingAssignments.FleetBookingAssignments where
  fromTType' (Beam.FleetBookingAssignmentsT {..}) = do
    pure $
      Just
        Domain.Types.FleetBookingAssignments.FleetBookingAssignments
          { amount = amount,
            assignmentEndTime = assignmentEndTime,
            assignmentStartTime = assignmentStartTime,
            bookingId = bookingId,
            createdAt = createdAt,
            fleetOwnerId = fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            mainAssignmentId = Kernel.Types.Id.Id mainAssignmentId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            paymentMethod = paymentMethod,
            placeName = placeName,
            serviceId = serviceId,
            serviceName = serviceName,
            skuDurationMins = skuDurationMins,
            updatedAt = updatedAt,
            vehicleNo = vehicleNo,
            visitDate = visitDate
          }

instance ToTType' Beam.FleetBookingAssignments Domain.Types.FleetBookingAssignments.FleetBookingAssignments where
  toTType' (Domain.Types.FleetBookingAssignments.FleetBookingAssignments {..}) = do
    Beam.FleetBookingAssignmentsT
      { Beam.amount = amount,
        Beam.assignmentEndTime = assignmentEndTime,
        Beam.assignmentStartTime = assignmentStartTime,
        Beam.bookingId = bookingId,
        Beam.createdAt = createdAt,
        Beam.fleetOwnerId = fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.mainAssignmentId = Kernel.Types.Id.getId mainAssignmentId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.paymentMethod = paymentMethod,
        Beam.placeName = placeName,
        Beam.serviceId = serviceId,
        Beam.serviceName = serviceName,
        Beam.skuDurationMins = skuDurationMins,
        Beam.updatedAt = updatedAt,
        Beam.vehicleNo = vehicleNo,
        Beam.visitDate = visitDate
      }
