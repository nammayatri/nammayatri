{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Storage.Beam.Merchant.DriverIntelligentPoolConfig where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Lib.Utils ()
import Sequelize

data DriverIntelligentPoolConfigT f = DriverIntelligentPoolConfigT
  { merchantId :: B.C f Text,
    actualPickupDistanceWeightage :: B.C f Int,
    availabilityTimeWeightage :: B.C f Int,
    availabilityTimeWindowOption :: B.C f SWC.SlidingWindowOptions,
    acceptanceRatioWeightage :: B.C f Int,
    acceptanceRatioWindowOption :: B.C f SWC.SlidingWindowOptions,
    cancellationRatioWeightage :: B.C f Int,
    cancellationRatioWindowOption :: B.C f SWC.SlidingWindowOptions,
    minQuotesToQualifyForIntelligentPool :: B.C f Int,
    minQuotesToQualifyForIntelligentPoolWindowOption :: B.C f SWC.SlidingWindowOptions,
    intelligentPoolPercentage :: B.C f (Maybe Int),
    speedNormalizer :: B.C f Double,
    driverSpeedWeightage :: B.C f Int,
    locationUpdateSampleTime :: B.C f Minutes,
    minLocationUpdates :: B.C f Int,
    defaultDriverSpeed :: B.C f Double,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverIntelligentPoolConfigT where
  data PrimaryKey DriverIntelligentPoolConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

type DriverIntelligentPoolConfig = DriverIntelligentPoolConfigT Identity

driverIntelligentPoolConfigTMod :: DriverIntelligentPoolConfigT (B.FieldModification (B.TableField DriverIntelligentPoolConfigT))
driverIntelligentPoolConfigTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      actualPickupDistanceWeightage = B.fieldNamed "actual_pickup_distance_weightage",
      availabilityTimeWeightage = B.fieldNamed "availability_time_weightage",
      availabilityTimeWindowOption = B.fieldNamed "availability_time_window_option",
      acceptanceRatioWeightage = B.fieldNamed "acceptance_ratio_weightage",
      acceptanceRatioWindowOption = B.fieldNamed "acceptance_ratio_window_option",
      cancellationRatioWeightage = B.fieldNamed "cancellation_ratio_weightage",
      cancellationRatioWindowOption = B.fieldNamed "cancellation_ratio_window_option",
      minQuotesToQualifyForIntelligentPool = B.fieldNamed "min_quotes_to_qualify_for_intelligent_pool",
      minQuotesToQualifyForIntelligentPoolWindowOption = B.fieldNamed "min_quotes_to_qualify_for_intelligent_pool_window_option",
      intelligentPoolPercentage = B.fieldNamed "intelligent_pool_percentage",
      speedNormalizer = B.fieldNamed "speed_normalizer",
      driverSpeedWeightage = B.fieldNamed "driver_speed_weightage",
      locationUpdateSampleTime = B.fieldNamed "location_update_sample_time",
      minLocationUpdates = B.fieldNamed "min_location_updates",
      defaultDriverSpeed = B.fieldNamed "default_driver_speed",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''DriverIntelligentPoolConfigT ['merchantId] [])

$(mkTableInstances ''DriverIntelligentPoolConfigT "driver_intelligent_pool_config" "atlas_driver_offer_bpp")
