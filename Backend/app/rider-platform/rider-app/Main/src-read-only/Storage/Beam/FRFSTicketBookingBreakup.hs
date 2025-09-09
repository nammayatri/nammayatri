{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSTicketBookingBreakup where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSQuoteCategorySpec
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSTicketBookingBreakupT f = FRFSTicketBookingBreakupT
  { id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    quoteCategoryId :: (B.C f Kernel.Prelude.Text),
    tag :: (B.C f Domain.Types.FRFSQuoteCategorySpec.FRFSCategoryTag),
    ticketBookingId :: (B.C f Kernel.Prelude.Text),
    value :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSTicketBookingBreakupT where
  data PrimaryKey FRFSTicketBookingBreakupT f = FRFSTicketBookingBreakupId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSTicketBookingBreakupId . id

type FRFSTicketBookingBreakup = FRFSTicketBookingBreakupT Identity

$(enableKVPG (''FRFSTicketBookingBreakupT) [('id)] [[('quoteCategoryId)]])

$(mkTableInstances (''FRFSTicketBookingBreakupT) "frfs_ticket_booking_breakup")
