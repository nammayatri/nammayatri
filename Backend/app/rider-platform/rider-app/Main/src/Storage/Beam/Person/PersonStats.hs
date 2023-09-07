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

module Storage.Beam.Person.PersonStats where

import qualified Database.Beam as B
import Kernel.Prelude
import Tools.Beam.UtilsTH

data PersonStatsT f = PersonStatsT
  { personId :: B.C f Text,
    userCancelledRides :: B.C f Int,
    driverCancelledRides :: B.C f Int,
    completedRides :: B.C f Int,
    weekendRides :: B.C f Int,
    weekdayRides :: B.C f Int,
    offPeakRides :: B.C f Int,
    eveningPeakRides :: B.C f Int,
    morningPeakRides :: B.C f Int,
    weekendPeakRides :: B.C f Int,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonStatsT where
  data PrimaryKey PersonStatsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . personId

type PersonStats = PersonStatsT Identity

$(enableKVPG ''PersonStatsT ['personId] [])
$(mkTableInstances ''PersonStatsT "person_stats")
