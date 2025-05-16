{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSTicketBooking where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSTicketBooking
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSTicketBookingT f = FRFSTicketBookingT
  { bapId :: (B.C f Kernel.Prelude.Text),
    bookingType :: (B.C f Domain.Types.FRFSTicketBooking.BookingTypeEnum),
    bppId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    quantity :: (B.C f Kernel.Prelude.Int),
    selectedFareId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    status :: (B.C f Domain.Types.FRFSTicketBooking.BookingStatusEnum),
    transactionId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSTicketBookingT where
  data PrimaryKey FRFSTicketBookingT f = FRFSTicketBookingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSTicketBookingId . id

type FRFSTicketBooking = FRFSTicketBookingT Identity

$(enableKVPG (''FRFSTicketBookingT) [('id)] [])

$(mkTableInstances (''FRFSTicketBookingT) "frfs_ticket_booking")
