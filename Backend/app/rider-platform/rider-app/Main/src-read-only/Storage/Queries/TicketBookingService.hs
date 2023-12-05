{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketBookingService where

import qualified Domain.Types.BusinessHour as Domain.Types.BusinessHour
import qualified Domain.Types.Merchant.MerchantOperatingCity as Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.TicketBooking as Domain.Types.TicketBooking
import qualified Domain.Types.TicketBookingService as Domain.Types.TicketBookingService
import qualified Domain.Types.TicketService as Domain.Types.TicketService
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Prelude as Kernel.Prelude
import qualified Kernel.Types.Common as Kernel.Types.Common
import qualified Kernel.Types.Id as Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketBookingService as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TicketBookingService.TicketBookingService -> m ()
create = createWithKV

findAllByBookingId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m ([Domain.Types.TicketBookingService.TicketBookingService])
findAllByBookingId (Kernel.Types.Id.Id ticketBookingId) = do
  findAllWithKV
    [ Se.Is Beam.ticketBookingId $ Se.Eq ticketBookingId
    ]

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService -> m (Maybe (Domain.Types.TicketBookingService.TicketBookingService))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

instance FromTType' Beam.TicketBookingService Domain.Types.TicketBookingService.TicketBookingService where
  fromTType' Beam.TicketBookingServiceT {..} = do
    pure $
      Just
        Domain.Types.TicketBookingService.TicketBookingService
          { amount = amount,
            btype = btype,
            createdAt = createdAt,
            expiryDate = expiryDate,
            id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            shortId = Kernel.Types.Id.ShortId shortId,
            status = status,
            ticketBookingId = Kernel.Types.Id.Id ticketBookingId,
            ticketServiceId = Kernel.Types.Id.Id ticketServiceId,
            updatedAt = updatedAt,
            verificationCount = verificationCount
          }

instance ToTType' Beam.TicketBookingService Domain.Types.TicketBookingService.TicketBookingService where
  toTType' Domain.Types.TicketBookingService.TicketBookingService {..} = do
    Beam.TicketBookingServiceT
      { Beam.amount = amount,
        Beam.btype = btype,
        Beam.createdAt = createdAt,
        Beam.expiryDate = expiryDate,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.status = status,
        Beam.ticketBookingId = Kernel.Types.Id.getId ticketBookingId,
        Beam.ticketServiceId = Kernel.Types.Id.getId ticketServiceId,
        Beam.updatedAt = updatedAt,
        Beam.verificationCount = verificationCount
      }
