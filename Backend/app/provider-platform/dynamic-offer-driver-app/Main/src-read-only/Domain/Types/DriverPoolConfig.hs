{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverPoolConfig where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config
import qualified Tools.Beam.UtilsTH

data DriverPoolConfig = DriverPoolConfig
  { actualDistanceThreshold :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    createdAt :: Kernel.Prelude.UTCTime,
    distanceBasedBatchSplit :: [SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config.BatchSplitByPickupDistance],
    driverBatchSize :: Kernel.Prelude.Int,
    driverPositionInfoExpiry :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    driverQuoteLimit :: Kernel.Prelude.Int,
    driverRequestCountLimit :: Kernel.Prelude.Int,
    driverToDestinationDistanceThreshold :: Kernel.Types.Common.Meters,
    driverToDestinationDuration :: Kernel.Types.Common.Seconds,
    maxDriverQuotesRequired :: Kernel.Prelude.Int,
    maxNumberOfBatches :: Kernel.Prelude.Int,
    maxParallelSearchRequests :: Kernel.Prelude.Int,
    maxRadiusOfSearch :: Kernel.Types.Common.Meters,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity,
    minRadiusOfSearch :: Kernel.Types.Common.Meters,
    poolSortingType :: SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config.PoolSortingType,
    radiusShrinkValueForDriversOnRide :: Kernel.Types.Common.Meters,
    radiusStepSize :: Kernel.Types.Common.Meters,
    singleBatchProcessTime :: Kernel.Types.Common.Seconds,
    tripCategory :: Kernel.Prelude.Text,
    tripDistance :: Kernel.Types.Common.Meters,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleVariant :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
