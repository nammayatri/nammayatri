{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FRFSTicketBookingFeedback where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data FRFSTicketBookingFeedbackT f
    = FRFSTicketBookingFeedbackT {bookingId :: (B.C f Kernel.Prelude.Text),
                                  createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                  feedbackDetails :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  id :: (B.C f Kernel.Prelude.Text),
                                  isFareAccepted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                                  merchantId :: (B.C f Kernel.Prelude.Text),
                                  merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                                  updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FRFSTicketBookingFeedbackT
    where data PrimaryKey FRFSTicketBookingFeedbackT f = FRFSTicketBookingFeedbackId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = FRFSTicketBookingFeedbackId . id
type FRFSTicketBookingFeedback = FRFSTicketBookingFeedbackT Identity

$(enableKVPG (''FRFSTicketBookingFeedbackT) [('id)] [[('bookingId)]])

$(mkTableInstances (''FRFSTicketBookingFeedbackT) "frfs_ticket_booking_feedback")

