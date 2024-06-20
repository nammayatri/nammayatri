{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.TicketBooking where

import qualified Domain.Types.TicketBooking
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.TicketBooking as Beam

instance FromTType' Beam.TicketBooking Domain.Types.TicketBooking.TicketBooking where
  fromTType' (Beam.TicketBookingT {..}) = do
    pure $
      Just
        Domain.Types.TicketBooking.TicketBooking
          { id = Kernel.Types.Id.Id id,
            shortId = Kernel.Types.Id.ShortId shortId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            ticketPlaceId = Kernel.Types.Id.Id ticketPlaceId,
            personId = Kernel.Types.Id.Id personId,
            amount = Kernel.Types.Common.mkPrice currency amount,
            visitDate = visitDate,
            status = status,
            createdAt = createdAt,
            updatedAt = updatedAt,
            bookedSeats = bookedSeats,
            cancelledSeats = cancelledSeats,
            merchantId = Kernel.Types.Id.Id <$> merchantId
          }

instance ToTType' Beam.TicketBooking Domain.Types.TicketBooking.TicketBooking where
  toTType' (Domain.Types.TicketBooking.TicketBooking {..}) = do
    Beam.TicketBookingT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.ticketPlaceId = Kernel.Types.Id.getId ticketPlaceId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.amount = (.amount) amount,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) amount,
        Beam.visitDate = visitDate,
        Beam.status = status,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt,
        Beam.bookedSeats = bookedSeats,
        Beam.cancelledSeats = cancelledSeats,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId
      }
