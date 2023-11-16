{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Tickets.TicketServicePrice where

import qualified Database.Beam as B
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data TicketServicePriceT f = TicketServicePriceT
  { ticketServiceId :: B.C f Text,
    attendeeType :: B.C f Text,
    pricePerUnit :: B.C f HighPrecMoney
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketServicePriceT where
  data PrimaryKey TicketServicePriceT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . ticketServiceId

type TicketServicePrice = TicketServicePriceT Identity

$(enableKVPG ''TicketServicePriceT ['ticketServiceId] [])

$(mkTableInstances ''TicketServicePriceT "ticket_service_price")
