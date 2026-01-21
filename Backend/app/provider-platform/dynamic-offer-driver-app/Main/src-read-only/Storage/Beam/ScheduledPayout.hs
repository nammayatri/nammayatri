{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ScheduledPayout where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.ScheduledPayout
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data ScheduledPayoutT f = ScheduledPayoutT
  { amount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    bookingId :: (B.C f Kernel.Prelude.Text),
    cancelReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    driverId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    retryCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    rideId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.ScheduledPayout.ScheduledPayoutStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table ScheduledPayoutT where
  data PrimaryKey ScheduledPayoutT f = ScheduledPayoutId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ScheduledPayoutId . id

type ScheduledPayout = ScheduledPayoutT Identity

$(enableKVPG (''ScheduledPayoutT) [('id)] [[('rideId)]])

$(mkTableInstances (''ScheduledPayoutT) "scheduled_payout")
