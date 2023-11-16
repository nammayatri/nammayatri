{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Tickets.TicketBookingServicePriceBreakup where

import Domain.Types.Tickets as DomainTBSPB
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id (Id, getId))
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Tickets.TicketBookingServicePriceBreakup as BeamTBS

createMany :: MonadFlow m => [TicketBookingServicePriceBreakup] -> m ()
createMany = mapM_ createWithKV

findByTicketBookingServiceId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m [TicketBookingServicePriceBreakup]
findByTicketBookingServiceId ticketBookingServiceId =
  findAllWithKV
    [ Se.Is BeamTBS.ticketBookingServiceId $ Se.Eq ticketBookingServiceId
    ]

instance FromTType' BeamTBS.TicketBookingServicePriceBreakup TicketBookingServicePriceBreakup where
  fromTType' BeamTBS.TicketBookingServicePriceBreakupT {..} =
    pure $
      Just $
        TicketBookingServicePriceBreakup
          { ticketBookingServiceId = Id ticketBookingServiceId,
            ..
          }

instance ToTType' BeamTBS.TicketBookingServicePriceBreakup TicketBookingServicePriceBreakup where
  toTType' TicketBookingServicePriceBreakup {..} =
    BeamTBS.TicketBookingServicePriceBreakupT
      { BeamTBS.ticketBookingServiceId = getId ticketBookingServiceId,
        BeamTBS.attendeeType = attendeeType,
        BeamTBS.numberOfUnits = numberOfUnits,
        BeamTBS.pricePerUnit = pricePerUnit
      }
