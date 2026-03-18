{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverGracePeriodTracker where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverGracePeriodTrackerT f = DriverGracePeriodTrackerT
  { id :: B.C f Kernel.Prelude.Text,
    driverId :: B.C f Kernel.Prelude.Text,
    ruleId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    offenseCount :: B.C f Kernel.Prelude.Int,
    windowStartTime :: B.C f Kernel.Prelude.UTCTime,
    windowEndTime :: B.C f Kernel.Prelude.UTCTime,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverGracePeriodTrackerT where
  data PrimaryKey DriverGracePeriodTrackerT f = DriverGracePeriodTrackerId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverGracePeriodTrackerId . id

type DriverGracePeriodTracker = DriverGracePeriodTrackerT Identity

$(enableKVPG ''DriverGracePeriodTrackerT ['id] [['driverId], ['ruleId]])

$(mkTableInstances ''DriverGracePeriodTrackerT "driver_grace_period_tracker")
