{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TicketBooking where

import qualified Data.Aeson
import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.TicketBooking
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data TicketBookingT f = TicketBookingT
  { amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    blockExpirationTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    bookedSeats :: B.C f Kernel.Prelude.Int,
    cancelledSeats :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    shortId :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.Extra.TicketBooking.BookingStatus,
    ticketPlaceId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vendorSplitDetails :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    visitDate :: B.C f Data.Time.Day,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketBookingT where
  data PrimaryKey TicketBookingT f = TicketBookingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TicketBookingId . id

type TicketBooking = TicketBookingT Identity

$(enableKVPG ''TicketBookingT ['id] [['personId], ['shortId]])

$(mkTableInstances ''TicketBookingT "ticket_booking")
