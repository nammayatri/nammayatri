{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.DriverStats where

import qualified Database.Beam as B
import Tools.Beam.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common hiding (id)

data DriverStatsT f = DriverStatsT
  { driverId :: B.C f Text,
    idleSince :: B.C f UTCTime,
    totalRides :: B.C f Int,
    totalEarnings :: B.C f Money,
    bonusEarned :: B.C f Money,
    lateNightTrips :: B.C f Int,
    earningsMissed :: B.C f Money,
    totalDistance :: B.C f Double,
    ridesCancelled :: B.C f (Maybe Int),
    totalRidesAssigned :: B.C f (Maybe Int),
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverStatsT where
  data PrimaryKey DriverStatsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

type DriverStats = DriverStatsT Identity

$(enableKVPG ''DriverStatsT ['driverId] [])

$(mkTableInstances ''DriverStatsT "driver_stats")
