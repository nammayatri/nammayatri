{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverPoolConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.TimeBound
import qualified Lib.Types.SpecialLocation
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config
import Tools.Beam.UtilsTH

data DriverPoolConfigT f = DriverPoolConfigT
  { actualDistanceThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    actualDistanceThresholdOnRide :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    area :: B.C f Lib.Types.SpecialLocation.Area,
    batchSizeOnRide :: B.C f Kernel.Prelude.Int,
    batchSizeOnRideWithStraightLineDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currentRideTripCategoryValidForForwardBatching :: B.C f [Kernel.Prelude.Text],
    distanceBasedBatchSplit :: B.C f [SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config.BatchSplitByPickupDistance],
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    driverBatchSize :: B.C f Kernel.Prelude.Int,
    driverPositionInfoExpiry :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    driverQuoteLimit :: B.C f Kernel.Prelude.Int,
    driverRequestCountLimit :: B.C f Kernel.Prelude.Int,
    driverToDestinationDistanceThreshold :: B.C f Kernel.Types.Common.Meters,
    driverToDestinationDuration :: B.C f Kernel.Types.Common.Seconds,
    dynamicBatchSize :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Int]),
    enableForwardBatching :: B.C f Kernel.Prelude.Bool,
    enableUnifiedPooling :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    id :: B.C f Kernel.Prelude.Text,
    maxDriverQuotesRequired :: B.C f Kernel.Prelude.Int,
    maxNumberOfBatches :: B.C f Kernel.Prelude.Int,
    maxParallelSearchRequests :: B.C f Kernel.Prelude.Int,
    maxParallelSearchRequestsOnRide :: B.C f Kernel.Prelude.Int,
    maxRadiusOfSearch :: B.C f Kernel.Types.Common.Meters,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    minRadiusOfSearch :: B.C f Kernel.Types.Common.Meters,
    onRideBatchSplitConfig :: B.C f [SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config.BatchSplitByPickupDistanceOnRide],
    onRideRadiusConfig :: B.C f [SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config.OnRideRadiusConfig],
    poolSortingType :: B.C f SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config.PoolSortingType,
    radiusShrinkValueForDriversOnRide :: B.C f Kernel.Types.Common.Meters,
    radiusStepSize :: B.C f Kernel.Types.Common.Meters,
    scheduleTryTimes :: B.C f [Kernel.Prelude.Int],
    selfRequestIfRiderIsDriver :: B.C f Kernel.Prelude.Bool,
    singleBatchProcessTime :: B.C f Kernel.Types.Common.Seconds,
    thresholdToIgnoreActualDistanceThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    timeBounds :: B.C f Kernel.Types.TimeBound.TimeBound,
    tripCategory :: B.C f Kernel.Prelude.Text,
    tripDistance :: B.C f Kernel.Types.Common.Meters,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    useOneToOneOsrmMapping :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    vehicleVariant :: B.C f (Kernel.Prelude.Maybe Domain.Types.Common.ServiceTierType)
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverPoolConfigT where
  data PrimaryKey DriverPoolConfigT f = DriverPoolConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverPoolConfigId . id

type DriverPoolConfig = DriverPoolConfigT Identity

$(enableKVPG ''DriverPoolConfigT ['id] [])

$(mkTableInstances ''DriverPoolConfigT "driver_pool_config")

$(Domain.Types.UtilsTH.mkCacParseInstance ''DriverPoolConfigT)
