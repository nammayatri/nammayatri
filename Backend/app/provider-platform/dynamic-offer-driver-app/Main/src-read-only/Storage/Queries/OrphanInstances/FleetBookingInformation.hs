{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FleetBookingInformation where

import qualified Domain.Types.FleetBookingInformation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FleetBookingInformation as Beam

instance FromTType' Beam.FleetBookingInformation Domain.Types.FleetBookingInformation.FleetBookingInformation where
  fromTType' (Beam.FleetBookingInformationT {..}) = do
    pure $
      Just
        Domain.Types.FleetBookingInformation.FleetBookingInformation
          { amount = amount,
            bookedSeats = bookedSeats,
            bookingId = bookingId,
            createdAt = createdAt,
            fleetOwnerId = Kernel.Types.Id.Id <$> fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            personId = Kernel.Types.Id.Id <$> personId,
            placeName = placeName,
            serviceId = serviceId,
            serviceName = serviceName,
            status = status,
            updatedAt = updatedAt,
            vehicleNo = vehicleNo,
            visitDate = visitDate
          }

instance ToTType' Beam.FleetBookingInformation Domain.Types.FleetBookingInformation.FleetBookingInformation where
  toTType' (Domain.Types.FleetBookingInformation.FleetBookingInformation {..}) = do
    Beam.FleetBookingInformationT
      { Beam.amount = amount,
        Beam.bookedSeats = bookedSeats,
        Beam.bookingId = bookingId,
        Beam.createdAt = createdAt,
        Beam.fleetOwnerId = Kernel.Types.Id.getId <$> fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId <$> personId,
        Beam.placeName = placeName,
        Beam.serviceId = serviceId,
        Beam.serviceName = serviceName,
        Beam.status = status,
        Beam.updatedAt = updatedAt,
        Beam.vehicleNo = vehicleNo,
        Beam.visitDate = visitDate
      }
