{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.TicketBookingService where

import qualified Domain.Types.TicketBookingService
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.TicketBookingService as Beam

instance FromTType' Beam.TicketBookingService Domain.Types.TicketBookingService.TicketBookingService where
  fromTType' (Beam.TicketBookingServiceT {..}) = do
    pure $
      Just
        Domain.Types.TicketBookingService.TicketBookingService
          { id = Kernel.Types.Id.Id id,
            shortId = Kernel.Types.Id.ShortId shortId,
            ticketBookingId = Kernel.Types.Id.Id ticketBookingId,
            ticketServiceId = Kernel.Types.Id.Id ticketServiceId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            amount = Kernel.Types.Common.mkPrice currency amount,
            status = status,
            verificationCount = verificationCount,
            visitDate = visitDate,
            btype = btype,
            bHourId = Kernel.Types.Id.Id <$> bHourId,
            expiryDate = expiryDate,
            createdAt = createdAt,
            updatedAt = updatedAt,
            bookedSeats = bookedSeats,
            cancelledSeats = cancelledSeats,
            merchantId = Kernel.Types.Id.Id <$> merchantId
          }

instance ToTType' Beam.TicketBookingService Domain.Types.TicketBookingService.TicketBookingService where
  toTType' (Domain.Types.TicketBookingService.TicketBookingService {..}) = do
    Beam.TicketBookingServiceT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.ticketBookingId = Kernel.Types.Id.getId ticketBookingId,
        Beam.ticketServiceId = Kernel.Types.Id.getId ticketServiceId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.amount = (.amount) amount,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) amount,
        Beam.status = status,
        Beam.verificationCount = verificationCount,
        Beam.visitDate = visitDate,
        Beam.btype = btype,
        Beam.bHourId = Kernel.Types.Id.getId <$> bHourId,
        Beam.expiryDate = expiryDate,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt,
        Beam.bookedSeats = bookedSeats,
        Beam.cancelledSeats = cancelledSeats,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId
      }
