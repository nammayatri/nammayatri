{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketBookingServiceCategory where

import qualified Domain.Types.TicketBookingService as Domain.Types.TicketBookingService
import qualified Domain.Types.TicketBookingServiceCategory as Domain.Types.TicketBookingServiceCategory
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Prelude as Kernel.Prelude
import qualified Kernel.Types.Common as Kernel.Types.Common
import qualified Kernel.Types.Id as Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketBookingServiceCategory as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory -> m ()
create = createWithKV

instance FromTType' Beam.TicketBookingServiceCategory Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory where
  fromTType' Beam.TicketBookingServiceCategoryT {..} = do
    pure $
      Just
        Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory
          { amount = amount,
            bookedSeats = bookedSeats,
            id = Kernel.Types.Id.Id id,
            name = name,
            ticketBookingServiceId = Kernel.Types.Id.Id ticketBookingServiceId
          }

instance ToTType' Beam.TicketBookingServiceCategory Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory where
  toTType' Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory {..} = do
    Beam.TicketBookingServiceCategoryT
      { Beam.amount = amount,
        Beam.bookedSeats = bookedSeats,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.ticketBookingServiceId = Kernel.Types.Id.getId ticketBookingServiceId
      }
