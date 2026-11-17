{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverNudge where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DriverNudge
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data DriverNudgeT f = DriverNudgeT
  { actualDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    destination :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    driverId :: B.C f Kernel.Prelude.Text,
    estimatedDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.DriverNudge.NudgeStatus,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverNudgeT where
  data PrimaryKey DriverNudgeT f = DriverNudgeId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverNudgeId . id

type DriverNudge = DriverNudgeT Identity

$(enableKVPG ''DriverNudgeT ['id] [['driverId]])

$(mkTableInstances ''DriverNudgeT "driver_nudge")
