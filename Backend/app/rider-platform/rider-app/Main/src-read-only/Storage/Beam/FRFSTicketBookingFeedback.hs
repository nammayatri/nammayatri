{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSTicketBookingFeedback where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSTicketBookingFeedbackT f = FRFSTicketBookingFeedbackT
  { bookingId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    feedbackDetails :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    isFareAccepted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSTicketBookingFeedbackT where
  data PrimaryKey FRFSTicketBookingFeedbackT f = FRFSTicketBookingFeedbackId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSTicketBookingFeedbackId . id

type FRFSTicketBookingFeedback = FRFSTicketBookingFeedbackT Identity

$(enableKVPG ''FRFSTicketBookingFeedbackT ['id] [['bookingId]])

$(mkTableInstances ''FRFSTicketBookingFeedbackT "frfs_ticket_booking_feedback")
