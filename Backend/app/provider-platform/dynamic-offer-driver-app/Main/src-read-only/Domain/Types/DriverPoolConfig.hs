{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverPoolConfig where

import Data.Aeson
import qualified Data.Vector
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Lib.Types.SpecialLocation
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config
import qualified Tools.Beam.UtilsTH

data DriverPoolConfig = DriverPoolConfig
  { actualDistanceThreshold :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    actualDistanceThresholdOnRide :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    area :: Lib.Types.SpecialLocation.Area,
    batchSizeOnRide :: Kernel.Prelude.Int,
    batchSizeOnRideWithStraightLineDistance :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    currentRideTripCategoryValidForForwardBatching :: [Kernel.Prelude.Text],
    distanceBasedBatchSplit :: [SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config.BatchSplitByPickupDistance],
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverBatchSize :: Kernel.Prelude.Int,
    driverPositionInfoExpiry :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    driverQuoteLimit :: Kernel.Prelude.Int,
    driverRequestCountLimit :: Kernel.Prelude.Int,
    driverToDestinationDistanceThreshold :: Kernel.Types.Common.Meters,
    driverToDestinationDuration :: Kernel.Types.Common.Seconds,
    dynamicBatchSize :: Data.Vector.Vector Kernel.Prelude.Int,
    enableForwardBatching :: Kernel.Prelude.Bool,
    enableUnifiedPooling :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.DriverPoolConfig.DriverPoolConfig,
    maxDriverQuotesRequired :: Kernel.Prelude.Int,
    maxNumberOfBatches :: Kernel.Prelude.Int,
    maxParallelSearchRequests :: Kernel.Prelude.Int,
    maxParallelSearchRequestsOnRide :: Kernel.Prelude.Int,
    maxRadiusOfSearch :: Kernel.Types.Common.Meters,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    minRadiusOfSearch :: Kernel.Types.Common.Meters,
    onRideBatchSplitConfig :: [SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config.BatchSplitByPickupDistanceOnRide],
    onRideRadiusConfig :: [SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config.OnRideRadiusConfig],
    poolSortingType :: SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config.PoolSortingType,
    radiusShrinkValueForDriversOnRide :: Kernel.Types.Common.Meters,
    radiusStepSize :: Kernel.Types.Common.Meters,
    scheduleTryTimes :: [Kernel.Prelude.Int],
    singleBatchProcessTime :: Kernel.Types.Common.Seconds,
    thresholdToIgnoreActualDistanceThreshold :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    tripCategory :: Kernel.Prelude.Text,
    tripDistance :: Kernel.Types.Common.Meters,
    updatedAt :: Kernel.Prelude.UTCTime,
    useOneToOneOsrmMapping :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    vehicleVariant :: Kernel.Prelude.Maybe Domain.Types.Common.ServiceTierType
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)
