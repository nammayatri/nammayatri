{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.TicketBookingServiceCategory where

import qualified Domain.Types.TicketBookingServiceCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.TicketBookingServiceCategory as Beam

instance FromTType' Beam.TicketBookingServiceCategory Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory where
  fromTType' (Beam.TicketBookingServiceCategoryT {..}) = do
    pure $
      Just
        Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory
          { id = Kernel.Types.Id.Id id,
            ticketBookingServiceId = Kernel.Types.Id.Id ticketBookingServiceId,
            name = name,
            bookedSeats = bookedSeats,
            amount = Kernel.Types.Common.mkPrice currency amount,
            eventCancelledBy = eventCancelledBy,
            cancelledSeats = cancelledSeats,
            amountToRefund = amountToRefund,
            serviceCategoryId = serviceCategoryId,
            visitDate = visitDate,
            btype = btype,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TicketBookingServiceCategory Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory where
  toTType' (Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory {..}) = do
    Beam.TicketBookingServiceCategoryT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.ticketBookingServiceId = Kernel.Types.Id.getId ticketBookingServiceId,
        Beam.name = name,
        Beam.bookedSeats = bookedSeats,
        Beam.amount = (.amount) amount,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) amount,
        Beam.eventCancelledBy = eventCancelledBy,
        Beam.cancelledSeats = cancelledSeats,
        Beam.amountToRefund = amountToRefund,
        Beam.serviceCategoryId = serviceCategoryId,
        Beam.visitDate = visitDate,
        Beam.btype = btype,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
