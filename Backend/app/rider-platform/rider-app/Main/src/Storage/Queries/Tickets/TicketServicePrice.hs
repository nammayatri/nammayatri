{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Tickets.TicketServicePrice where

import Domain.Types.Tickets.TicketService as DomainTSP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Tickets.TicketServicePrice as BeamTSP

findByTicketServiceId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m [TicketServicePrice]
findByTicketServiceId ticketServiceId =
  findAllWithKV
    [ Se.Is BeamTSP.ticketServiceId $ Se.Eq ticketServiceId
    ]

instance FromTType' BeamTSP.TicketServicePrice TicketServicePrice where
  fromTType' BeamTSP.TicketServicePriceT {..} = do
    pure $
      Just $
        TicketServicePrice
          { ..
          }

instance ToTType' BeamTSP.TicketServicePrice TicketServicePrice where
  toTType' TicketServicePrice {..} =
    BeamTSP.TicketServicePriceT
      { BeamTSP.ticketServiceId = ticketServiceId,
        BeamTSP.attendeeType = attendeeType,
        BeamTSP.pricePerUnit = pricePerUnit
      }
