{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ScheduledBookingOpsNote where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.OpsNote
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ScheduledBookingOpsNoteT f = ScheduledBookingOpsNoteT
  { bookingId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    content :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    createdByDashboardUserId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    noteType :: (B.C f Domain.Types.OpsNote.OpsNoteType),
    status :: (B.C f Domain.Types.OpsNote.OpsNoteStatus),
    transactionId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table ScheduledBookingOpsNoteT where
  data PrimaryKey ScheduledBookingOpsNoteT f = ScheduledBookingOpsNoteId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ScheduledBookingOpsNoteId . id

type ScheduledBookingOpsNote = ScheduledBookingOpsNoteT Identity

$(enableKVPG (''ScheduledBookingOpsNoteT) [('id)] [[('transactionId)]])

$(mkTableInstances (''ScheduledBookingOpsNoteT) "scheduled_booking_ops_note")
