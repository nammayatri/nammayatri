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

module Storage.Beam.DriverStats where

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

data DriverStatsT f = DriverStatsT
  { driverId :: B.C f Text,
    idleSince :: B.C f Time.UTCTime,
    totalRides :: B.C f Int,
    totalEarnings :: B.C f Money,
    bonusEarned :: B.C f Money,
    lateNightTrips :: B.C f Int,
    earningsMissed :: B.C f Money,
    totalDistance :: B.C f Double,
    ridesCancelled :: B.C f (Maybe Int),
    totalRidesAssigned :: B.C f (Maybe Int),
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverStatsT where
  data PrimaryKey DriverStatsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

type DriverStats = DriverStatsT Identity

driverStatsTMod :: DriverStatsT (B.FieldModification (B.TableField DriverStatsT))
driverStatsTMod =
  B.tableModification
    { driverId = B.fieldNamed "driver_id",
      idleSince = B.fieldNamed "idle_since",
      totalRides = B.fieldNamed "total_rides",
      totalDistance = B.fieldNamed "total_distance",
      ridesCancelled = B.fieldNamed "rides_cancelled",
      totalRidesAssigned = B.fieldNamed "total_rides_assigned",
      totalEarnings = B.fieldNamed "total_earnings",
      bonusEarned = B.fieldNamed "bonus_earned",
      lateNightTrips = B.fieldNamed "late_night_trips",
      earningsMissed = B.fieldNamed "earnings_missed",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''DriverStatsT ['driverId] [])

$(mkTableInstances ''DriverStatsT "driver_stats" "atlas_driver_offer_bpp")
