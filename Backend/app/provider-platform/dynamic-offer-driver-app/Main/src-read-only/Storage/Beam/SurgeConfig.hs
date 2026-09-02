{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SurgeConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.SurgeConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.TimeBound
import Tools.Beam.UtilsTH

data SurgeConfigT f = SurgeConfigT
  { applyOnExtraDistanceOnly :: B.C f Kernel.Prelude.Bool,
    createdBy :: B.C f Kernel.Prelude.Text,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    excludedAreas :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    maxDeltaPerUpdate :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    maxMultiplier :: B.C f Kernel.Types.Common.Centesimal,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    minMultiplier :: B.C f Kernel.Types.Common.Centesimal,
    rows :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.SurgeConfig.SurgeConfigStatus,
    timeBounds :: B.C f Kernel.Types.TimeBound.TimeBound,
    vehicleServiceTier :: B.C f Domain.Types.Common.ServiceTierType,
    version :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SurgeConfigT where
  data PrimaryKey SurgeConfigT f = SurgeConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SurgeConfigId . id

type SurgeConfig = SurgeConfigT Identity

$(enableKVPG ''SurgeConfigT ['id] [])

$(mkTableInstances ''SurgeConfigT "surge_config")
