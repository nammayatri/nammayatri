{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.NavigationPointConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data NavigationPointConfigT f = NavigationPointConfigT
  { bucketGeohashPrecision :: (B.C f Kernel.Prelude.Int),
    enabled :: (B.C f Kernel.Prelude.Bool),
    maxPointsToShow :: (B.C f Kernel.Prelude.Int),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    pointGeohashPrecision :: (B.C f Kernel.Prelude.Int),
    radiusInMeters :: (B.C f Kernel.Prelude.Double),
    requiredFieldMask :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    ttlInSeconds :: (B.C f Kernel.Prelude.Int),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table NavigationPointConfigT where
  data PrimaryKey NavigationPointConfigT f = NavigationPointConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = NavigationPointConfigId . merchantOperatingCityId

type NavigationPointConfig = NavigationPointConfigT Identity

$(enableKVPG (''NavigationPointConfigT) [('merchantOperatingCityId)] [])

$(mkTableInstances (''NavigationPointConfigT) "navigation_point_config")
