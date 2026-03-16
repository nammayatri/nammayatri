{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.TicketBooking where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Domain.Types.Extra.TicketBooking
import qualified Data.Time
import qualified Data.Aeson
import qualified Database.Beam as B



data TicketBookingT f
    = TicketBookingT {amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                      currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
                      blockExpirationTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
                      bookedSeats :: (B.C f Kernel.Prelude.Int),
                      cancelledSeats :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                      createdAt :: (B.C f Kernel.Prelude.UTCTime),
                      id :: (B.C f Kernel.Prelude.Text),
                      merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                      paymentMethod :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.TicketBooking.PaymentMethod)),
                      peopleTicketQuantity :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                      personId :: (B.C f Kernel.Prelude.Text),
                      shortId :: (B.C f Kernel.Prelude.Text),
                      status :: (B.C f Domain.Types.Extra.TicketBooking.BookingStatus),
                      ticketBookedBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      ticketPlaceId :: (B.C f Kernel.Prelude.Text),
                      ticketSubPlaceId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                      updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                      vendorSplitDetails :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                      visitDate :: (B.C f Data.Time.Day),
                      merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table TicketBookingT
    where data PrimaryKey TicketBookingT f = TicketBookingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = TicketBookingId . id
type TicketBooking = TicketBookingT Identity

$(enableKVPG (''TicketBookingT) [('id)] [[('personId)], [('shortId)]])

$(mkTableInstances (''TicketBookingT) "ticket_booking")

