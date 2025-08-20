{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverIntelligentPoolConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.SlidingWindowCounters
import Tools.Beam.UtilsTH

data DriverIntelligentPoolConfigT f = DriverIntelligentPoolConfigT
  { acceptanceRatioWeightage :: B.C f Kernel.Prelude.Int,
    acceptanceRatioWindowOption :: B.C f Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    actualPickupDistanceWeightage :: B.C f Kernel.Prelude.Int,
    availabilityTimeWeightage :: B.C f Kernel.Prelude.Int,
    availabilityTimeWindowOption :: B.C f Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    cancellationAndRideFrequencyRatioWindowOption :: B.C f Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    cancellationRatioWeightage :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    defaultDriverSpeed :: B.C f Kernel.Prelude.Double,
    driverSpeedWeightage :: B.C f Kernel.Prelude.Int,
    intelligentPoolPercentage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    locationUpdateSampleTime :: B.C f Kernel.Types.Common.Minutes,
    maxNumRides :: B.C f Kernel.Prelude.Int,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    minLocationUpdates :: B.C f Kernel.Prelude.Int,
    minQuotesToQualifyForIntelligentPool :: B.C f Kernel.Prelude.Int,
    minQuotesToQualifyForIntelligentPoolWindowOption :: B.C f Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    numRidesWeightage :: B.C f Kernel.Prelude.Int,
    speedNormalizer :: B.C f Kernel.Prelude.Double,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverIntelligentPoolConfigT where
  data PrimaryKey DriverIntelligentPoolConfigT f = DriverIntelligentPoolConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverIntelligentPoolConfigId . merchantOperatingCityId

type DriverIntelligentPoolConfig = DriverIntelligentPoolConfigT Identity

$(enableKVPG ''DriverIntelligentPoolConfigT ['merchantOperatingCityId] [])

$(mkTableInstances ''DriverIntelligentPoolConfigT "driver_intelligent_pool_config")

$(Domain.Types.UtilsTH.mkCacParseInstance ''DriverIntelligentPoolConfigT)
