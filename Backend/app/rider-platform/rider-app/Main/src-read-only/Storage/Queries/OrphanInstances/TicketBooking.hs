{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.TicketBooking where

import qualified Data.Aeson
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
          { amount = Kernel.Types.Common.mkPrice currency amount,
            blockExpirationTime = blockExpirationTime,
            bookedSeats = bookedSeats,
            cancelledSeats = cancelledSeats,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            paymentMethod = paymentMethod,
            peopleTicketQuantity = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< peopleTicketQuantity,
            personId = Kernel.Types.Id.Id personId,
            shortId = Kernel.Types.Id.ShortId shortId,
            status = status,
            ticketBookedBy = ticketBookedBy,
            ticketPlaceId = Kernel.Types.Id.Id ticketPlaceId,
            ticketSubPlaceId = Kernel.Types.Id.Id <$> ticketSubPlaceId,
            updatedAt = updatedAt,
            vendorSplitDetails = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< vendorSplitDetails,
            visitDate = visitDate,
            merchantId = Kernel.Types.Id.Id <$> merchantId
          }

instance ToTType' Beam.TicketBooking Domain.Types.TicketBooking.TicketBooking where
  toTType' (Domain.Types.TicketBooking.TicketBooking {..}) = do
    Beam.TicketBookingT
      { Beam.amount = (.amount) amount,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) amount,
        Beam.blockExpirationTime = blockExpirationTime,
        Beam.bookedSeats = bookedSeats,
        Beam.cancelledSeats = cancelledSeats,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.paymentMethod = paymentMethod,
        Beam.peopleTicketQuantity = Data.Aeson.toJSON <$> peopleTicketQuantity,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.status = status,
        Beam.ticketBookedBy = ticketBookedBy,
        Beam.ticketPlaceId = Kernel.Types.Id.getId ticketPlaceId,
        Beam.ticketSubPlaceId = Kernel.Types.Id.getId <$> ticketSubPlaceId,
        Beam.updatedAt = updatedAt,
        Beam.vendorSplitDetails = Data.Aeson.toJSON <$> vendorSplitDetails,
        Beam.visitDate = visitDate,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId
      }
