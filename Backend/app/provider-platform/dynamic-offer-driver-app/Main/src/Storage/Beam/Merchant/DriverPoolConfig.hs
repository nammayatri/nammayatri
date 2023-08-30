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

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config (BatchSplitByPickupDistance (..), PoolSortingType (..))

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
    tripDistance :: B.C f Meters,
    radiusShrinkValueForDriversOnRide :: B.C f Meters,
    driverToDestinationDistanceThreshold :: B.C f Meters,
    driverToDestinationDuration :: B.C f Seconds,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverPoolConfigT where
  data PrimaryKey DriverPoolConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

type DriverPoolConfig = DriverPoolConfigT Identity

driverPoolConfigTMod :: DriverPoolConfigT (B.FieldModification (B.TableField DriverPoolConfigT))
driverPoolConfigTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      distanceBasedBatchSplit = B.fieldNamed "distance_based_batch_split",
      minRadiusOfSearch = B.fieldNamed "min_radius_of_search",
      maxRadiusOfSearch = B.fieldNamed "max_radius_of_search",
      radiusStepSize = B.fieldNamed "radius_step_size",
      driverPositionInfoExpiry = B.fieldNamed "driver_position_info_expiry",
      actualDistanceThreshold = B.fieldNamed "actual_distance_threshold",
      maxDriverQuotesRequired = B.fieldNamed "max_driver_quotes_required",
      maxParallelSearchRequests = B.fieldNamed "max_parallel_search_requests",
      driverQuoteLimit = B.fieldNamed "driver_quote_limit",
      driverRequestCountLimit = B.fieldNamed "driver_request_count_limit",
      driverBatchSize = B.fieldNamed "driver_batch_size",
      maxNumberOfBatches = B.fieldNamed "max_number_of_batches",
      poolSortingType = B.fieldNamed "pool_sorting_type",
      singleBatchProcessTime = B.fieldNamed "single_batch_process_time",
      tripDistance = B.fieldNamed "trip_distance",
      radiusShrinkValueForDriversOnRide = B.fieldNamed "radius_shrink_value_for_drivers_on_ride",
      driverToDestinationDistanceThreshold = B.fieldNamed "driver_to_destination_distance_threshold",
      driverToDestinationDuration = B.fieldNamed "driver_to_destination_duration",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''DriverPoolConfigT ['merchantId] [])

$(mkTableInstances ''DriverPoolConfigT "driver_pool_config" "atlas_driver_offer_bpp")
