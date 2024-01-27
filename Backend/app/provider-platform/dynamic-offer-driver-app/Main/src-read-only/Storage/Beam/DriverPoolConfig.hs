{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverPoolConfig where

import qualified Database.Beam as B
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config
import Tools.Beam.UtilsTH

data DriverPoolConfigT f = DriverPoolConfigT
  { actualDistanceThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    distanceBasedBatchSplit :: B.C f [SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config.BatchSplitByPickupDistance],
    driverBatchSize :: B.C f Kernel.Prelude.Int,
    driverPositionInfoExpiry :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    driverQuoteLimit :: B.C f Kernel.Prelude.Int,
    driverRequestCountLimit :: B.C f Kernel.Prelude.Int,
    driverToDestinationDistanceThreshold :: B.C f Kernel.Types.Common.Meters,
    driverToDestinationDuration :: B.C f Kernel.Types.Common.Seconds,
    maxDriverQuotesRequired :: B.C f Kernel.Prelude.Int,
    maxNumberOfBatches :: B.C f Kernel.Prelude.Int,
    maxParallelSearchRequests :: B.C f Kernel.Prelude.Int,
    maxRadiusOfSearch :: B.C f Kernel.Types.Common.Meters,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    minRadiusOfSearch :: B.C f Kernel.Types.Common.Meters,
    poolSortingType :: B.C f SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config.PoolSortingType,
    radiusShrinkValueForDriversOnRide :: B.C f Kernel.Types.Common.Meters,
    radiusStepSize :: B.C f Kernel.Types.Common.Meters,
    singleBatchProcessTime :: B.C f Kernel.Types.Common.Seconds,
    tripCategory :: B.C f Kernel.Prelude.Text,
    tripDistance :: B.C f Kernel.Types.Common.Meters,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleVariant :: B.C f Kernel.Prelude.Text
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverPoolConfigT where
  data PrimaryKey DriverPoolConfigT f = DriverPoolConfigId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Types.Common.Meters) (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = DriverPoolConfigId <$> merchantOperatingCityId <*> tripCategory <*> tripDistance <*> vehicleVariant

type DriverPoolConfig = DriverPoolConfigT Identity

$(enableKVPG ''DriverPoolConfigT ['merchantOperatingCityId, 'tripCategory, 'tripDistance, 'vehicleVariant] [])

$(mkTableInstances ''DriverPoolConfigT "driver_pool_config")
