{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSBookingGroup where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSBookingGroup
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSBookingGroupT f = FRFSBookingGroupT
  { id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    paymentOrderShortId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    riderId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.FRFSBookingGroup.FRFSBookingGroupStatus),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    price :: (B.C f Kernel.Types.Common.HighPrecMoney),
    totalSlots :: (B.C f Kernel.Prelude.Int),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSBookingGroupT where
  data PrimaryKey FRFSBookingGroupT f = FRFSBookingGroupId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSBookingGroupId . id

type FRFSBookingGroup = FRFSBookingGroupT Identity

$(enableKVPG (''FRFSBookingGroupT) [('id)] [])

$(mkTableInstances (''FRFSBookingGroupT) "frfs_booking_group")
