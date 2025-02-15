{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.TicketBookingServiceCategory where

import qualified Data.Aeson
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
          { amount = Kernel.Types.Common.mkPrice currency amount,
            amountToRefund = amountToRefund,
            bookedSeats = bookedSeats,
            btype = btype,
            cancelledSeats = cancelledSeats,
            eventCancelledBy = eventCancelledBy,
            id = Kernel.Types.Id.Id id,
            name = name,
            serviceCategoryId = serviceCategoryId,
            ticketBookingServiceId = Kernel.Types.Id.Id ticketBookingServiceId,
            vendorSplitDetails = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< vendorSplitDetails,
            visitDate = visitDate,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TicketBookingServiceCategory Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory where
  toTType' (Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory {..}) = do
    Beam.TicketBookingServiceCategoryT
      { Beam.amount = (.amount) amount,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) amount,
        Beam.amountToRefund = amountToRefund,
        Beam.bookedSeats = bookedSeats,
        Beam.btype = btype,
        Beam.cancelledSeats = cancelledSeats,
        Beam.eventCancelledBy = eventCancelledBy,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.serviceCategoryId = serviceCategoryId,
        Beam.ticketBookingServiceId = Kernel.Types.Id.getId ticketBookingServiceId,
        Beam.vendorSplitDetails = Data.Aeson.toJSON <$> vendorSplitDetails,
        Beam.visitDate = visitDate,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
