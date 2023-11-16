{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Storage.Beam.Tickets.TicketBookingServicePriceBreakup where

import qualified Database.Beam as B
import Kernel.Prelude
import Kernel.Utils.Common (HighPrecMoney)
import Tools.Beam.UtilsTH

data TicketBookingServicePriceBreakupT f = TicketBookingServicePriceBreakupT
  { ticketBookingServiceId :: B.C f Text,
    attendeeType :: B.C f Text,
    numberOfUnits :: B.C f Int,
    pricePerUnit :: B.C f HighPrecMoney
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketBookingServicePriceBreakupT where
  data PrimaryKey TicketBookingServicePriceBreakupT f = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . ticketBookingServiceId

type TicketBookingServicePriceBreakup = TicketBookingServicePriceBreakupT Identity

$(enableKVPG ''TicketBookingServicePriceBreakupT ['ticketBookingServiceId] [])

$(mkTableInstances ''TicketBookingServicePriceBreakupT "ticket_booking_service_price_breakup")
