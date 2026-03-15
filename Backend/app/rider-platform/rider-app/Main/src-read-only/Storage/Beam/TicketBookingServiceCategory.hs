{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.TicketBookingServiceCategory where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Domain.Types.BusinessHour
import qualified Domain.Types.TicketBookingServiceCategory
import qualified Data.Time
import qualified Data.Aeson
import qualified Database.Beam as B



data TicketBookingServiceCategoryT f
    = TicketBookingServiceCategoryT {amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                                     currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
                                     amountToRefund :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                                     bookedSeats :: (B.C f Kernel.Prelude.Int),
                                     btype :: (B.C f (Kernel.Prelude.Maybe Domain.Types.BusinessHour.BusinessHourType)),
                                     cancelledSeats :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                                     eventCancelledBy :: (B.C f (Kernel.Prelude.Maybe Domain.Types.TicketBookingServiceCategory.CancelledBy)),
                                     id :: (B.C f Kernel.Prelude.Text),
                                     name :: (B.C f Kernel.Prelude.Text),
                                     serviceCategoryId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                     ticketBookingServiceId :: (B.C f Kernel.Prelude.Text),
                                     vendorSplitDetails :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                                     visitDate :: (B.C f (Kernel.Prelude.Maybe Data.Time.Day)),
                                     merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                     merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                     createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                     updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table TicketBookingServiceCategoryT
    where data PrimaryKey TicketBookingServiceCategoryT f = TicketBookingServiceCategoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = TicketBookingServiceCategoryId . id
type TicketBookingServiceCategory = TicketBookingServiceCategoryT Identity

$(enableKVPG (''TicketBookingServiceCategoryT) [('id)] [])

$(mkTableInstances (''TicketBookingServiceCategoryT) "ticket_booking_service_category")

