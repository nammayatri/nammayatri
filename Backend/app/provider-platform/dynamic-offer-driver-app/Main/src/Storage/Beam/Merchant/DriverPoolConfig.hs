{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.Merchant.DriverPoolConfig where

import qualified Database.Beam as B
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config (BatchSplitByPickupDistance (..), PoolSortingType (..))
import Tools.Beam.UtilsTH

data DriverPoolConfigT f = DriverPoolConfigT
  { merchantId :: B.C f Text,
    distanceBasedBatchSplit :: B.C f [BatchSplitByPickupDistance], -- (PostgresList BatchSplitByPickupDistance)
    minRadiusOfSearch :: B.C f Meters,
    maxRadiusOfSearch :: B.C f Meters,
    radiusStepSize :: B.C f Meters,
    driverPositionInfoExpiry :: B.C f (Maybe Seconds),
    actualDistanceThreshold :: B.C f (Maybe Meters),
    maxDriverQuotesRequired :: B.C f Int,
    maxParallelSearchRequests :: B.C f Int,
    driverQuoteLimit :: B.C f Int,
    driverRequestCountLimit :: B.C f Int,
    driverBatchSize :: B.C f Int,
    maxNumberOfBatches :: B.C f Int,
    poolSortingType :: B.C f PoolSortingType,
    singleBatchProcessTime :: B.C f Seconds,
    singleBatchProcessTimeRental :: B.C f Seconds,
    tripDistance :: B.C f Meters,
    radiusShrinkValueForDriversOnRide :: B.C f Meters,
    driverToDestinationDistanceThreshold :: B.C f Meters,
    driverToDestinationDuration :: B.C f Seconds,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverPoolConfigT where
  data PrimaryKey DriverPoolConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

type DriverPoolConfig = DriverPoolConfigT Identity

$(enableKVPG ''DriverPoolConfigT ['merchantId] [])

$(mkTableInstances ''DriverPoolConfigT "driver_pool_config")
