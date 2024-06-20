{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TicketBooking where

import qualified Data.Time.Calendar
import qualified Database.Beam as B
import qualified Domain.Types.TicketBooking
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data TicketBookingT f = TicketBookingT
  { id :: B.C f Kernel.Prelude.Text,
    shortId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    ticketPlaceId :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    visitDate :: B.C f Data.Time.Calendar.Day,
    status :: B.C f Domain.Types.TicketBooking.BookingStatus,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    bookedSeats :: B.C f Kernel.Prelude.Int,
    cancelledSeats :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketBookingT where
  data PrimaryKey TicketBookingT f = TicketBookingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TicketBookingId . id

type TicketBooking = TicketBookingT Identity

$(enableKVPG ''TicketBookingT ['id] [['shortId], ['personId]])

$(mkTableInstances ''TicketBookingT "ticket_booking")
