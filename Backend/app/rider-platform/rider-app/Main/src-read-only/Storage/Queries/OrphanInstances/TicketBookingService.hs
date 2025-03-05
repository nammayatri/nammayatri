{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.TicketBookingService where

import qualified Data.Aeson
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
          { amount = Kernel.Types.Common.mkPrice currency amount,
            bHourId = Kernel.Types.Id.Id <$> bHourId,
            bookedSeats = bookedSeats,
            btype = btype,
            cancelledSeats = cancelledSeats,
            createdAt = createdAt,
            expiryDate = expiryDate,
            id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            shortId = Kernel.Types.Id.ShortId shortId,
            status = status,
            ticketBookingId = Kernel.Types.Id.Id ticketBookingId,
            ticketServiceId = Kernel.Types.Id.Id ticketServiceId,
            updatedAt = updatedAt,
            vendorSplitDetails = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< vendorSplitDetails,
            verificationCount = verificationCount,
            visitDate = visitDate,
            merchantId = Kernel.Types.Id.Id <$> merchantId
          }

instance ToTType' Beam.TicketBookingService Domain.Types.TicketBookingService.TicketBookingService where
  toTType' (Domain.Types.TicketBookingService.TicketBookingService {..}) = do
    Beam.TicketBookingServiceT
      { Beam.amount = (.amount) amount,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) amount,
        Beam.bHourId = Kernel.Types.Id.getId <$> bHourId,
        Beam.bookedSeats = bookedSeats,
        Beam.btype = btype,
        Beam.cancelledSeats = cancelledSeats,
        Beam.createdAt = createdAt,
        Beam.expiryDate = expiryDate,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.status = status,
        Beam.ticketBookingId = Kernel.Types.Id.getId ticketBookingId,
        Beam.ticketServiceId = Kernel.Types.Id.getId ticketServiceId,
        Beam.updatedAt = updatedAt,
        Beam.vendorSplitDetails = Data.Aeson.toJSON <$> vendorSplitDetails,
        Beam.verificationCount = verificationCount,
        Beam.visitDate = visitDate,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId
      }
