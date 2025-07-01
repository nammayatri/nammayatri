{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSBookingFareBreakUp where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSBookingFareBreakUp
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSBookingFareBreakUpT f = FRFSBookingFareBreakUpT
  { amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    bookingId :: (B.C f Kernel.Prelude.Text),
    description :: (B.C f Domain.Types.FRFSBookingFareBreakUp.BookingBreakupDescription),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSBookingFareBreakUpT where
  data PrimaryKey FRFSBookingFareBreakUpT f = FRFSBookingFareBreakUpId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSBookingFareBreakUpId . id

type FRFSBookingFareBreakUp = FRFSBookingFareBreakUpT Identity

$(enableKVPG (''FRFSBookingFareBreakUpT) [('id)] [[('bookingId)]])

$(mkTableInstances (''FRFSBookingFareBreakUpT) "frfs_booking_fare_break_up")
