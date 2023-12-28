{-
    Copyright 2022-23, Juspay India Pvt Ltd

    This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

    as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

    is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

    or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

    the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Consumer.AvailabilityTime.Storage.Beam.Tables where

import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)

data DriverAvailabilityT f = DriverAvailabilityT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    merchantId :: B.C f Text,
    totalAvailableTime :: B.C f Int,
    lastAvailableTime :: B.C f Time.UTCTime,
    bucketStartTime :: B.C f Time.UTCTime,
    bucketEndTime :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverAvailabilityT where
  data PrimaryKey DriverAvailabilityT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DriverAvailability = DriverAvailabilityT Identity

$(enableKVPG ''DriverAvailabilityT ['id] [['driverId]])

$(mkTableInstances ''DriverAvailabilityT "driver_availability" "atlas_driver_offer_bpp")
