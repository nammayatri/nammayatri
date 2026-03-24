{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.TicketBookingService where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Domain.Types.BusinessHour
import qualified Domain.Types.TicketBookingService
import qualified Data.Time
import qualified Data.Aeson
import qualified Database.Beam as B



data TicketBookingServiceT f
    = TicketBookingServiceT {amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                             currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
                             assignmentId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                             bHourId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                             bookedSeats :: (B.C f Kernel.Prelude.Int),
                             btype :: (B.C f Domain.Types.BusinessHour.BusinessHourType),
                             cancelledSeats :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                             createdAt :: (B.C f Kernel.Prelude.UTCTime),
                             expiryDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                             id :: (B.C f Kernel.Prelude.Text),
                             merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                             shortId :: (B.C f Kernel.Prelude.Text),
                             status :: (B.C f Domain.Types.TicketBookingService.ServiceStatus),
                             ticketBookingId :: (B.C f Kernel.Prelude.Text),
                             ticketServiceId :: (B.C f Kernel.Prelude.Text),
                             updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                             vendorSplitDetails :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                             verificationCount :: (B.C f Kernel.Prelude.Int),
                             visitDate :: (B.C f (Kernel.Prelude.Maybe Data.Time.Day)),
                             merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table TicketBookingServiceT
    where data PrimaryKey TicketBookingServiceT f = TicketBookingServiceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = TicketBookingServiceId . id
type TicketBookingService = TicketBookingServiceT Identity

$(enableKVPG (''TicketBookingServiceT) [('id)] [])

$(mkTableInstances (''TicketBookingServiceT) "ticket_booking_service")

