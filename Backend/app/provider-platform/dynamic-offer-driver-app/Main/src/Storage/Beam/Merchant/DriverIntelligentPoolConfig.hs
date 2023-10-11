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

module Storage.Beam.Merchant.DriverIntelligentPoolConfig where

import qualified Database.Beam as B
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Tools.Beam.UtilsTH

data DriverIntelligentPoolConfigT f = DriverIntelligentPoolConfigT
  { merchantId :: B.C f Text,
    merchantOperatingCityId :: B.C f Text,
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
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverIntelligentPoolConfigT where
  data PrimaryKey DriverIntelligentPoolConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantOperatingCityId

type DriverIntelligentPoolConfig = DriverIntelligentPoolConfigT Identity

$(enableKVPG ''DriverIntelligentPoolConfigT ['merchantOperatingCityId] [])

$(mkTableInstances ''DriverIntelligentPoolConfigT "driver_intelligent_pool_config")
