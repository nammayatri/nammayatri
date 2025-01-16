{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.GoHomeConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data GoHomeConfigT f = GoHomeConfigT
  { activeTime :: B.C f Kernel.Prelude.Int,
    addStartWaypointAt :: B.C f Kernel.Prelude.Int,
    cancellationCnt :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    destRadiusMeters :: B.C f Kernel.Prelude.Int,
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    enableGoHome :: B.C f Kernel.Prelude.Bool,
    goHomeBatchDelay :: B.C f Kernel.Prelude.Int,
    goHomeFromLocationRadius :: B.C f Kernel.Types.Common.Meters,
    goHomeWayPointRadius :: B.C f Kernel.Types.Common.Meters,
    ignoreWaypointsTill :: B.C f Kernel.Prelude.Int,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    newLocAllowedRadius :: B.C f Kernel.Prelude.Int,
    numDriversForDirCheck :: B.C f Kernel.Prelude.Int,
    numHomeLocations :: B.C f Kernel.Prelude.Int,
    startCnt :: B.C f Kernel.Prelude.Int,
    updateHomeLocationAfterSec :: B.C f Kernel.Prelude.Int,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table GoHomeConfigT where
  data PrimaryKey GoHomeConfigT f = GoHomeConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = GoHomeConfigId . merchantOperatingCityId

type GoHomeConfig = GoHomeConfigT Identity

$(enableKVPG ''GoHomeConfigT ['merchantOperatingCityId] [])

$(mkTableInstances ''GoHomeConfigT "go_home_config")

$(Domain.Types.UtilsTH.mkCacParseInstance ''GoHomeConfigT)
