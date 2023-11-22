{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.DailyStats where

import Data.Time (Day)
import qualified Database.Beam as B
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data DailyStatsT f = DailyStatsT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    totalEarnings :: B.C f Money,
    numRides :: B.C f Int,
    totalDistance :: B.C f Meters,
    merchantLocalDate :: B.C f Day
  }
  deriving (Generic, B.Beamable)

instance B.Table DailyStatsT where
  data PrimaryKey DailyStatsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

type DailyStats = DailyStatsT Identity

$(enableKVPG ''DailyStatsT ['id] [['driverId]])

$(mkTableInstances ''DailyStatsT "daily_stats")
