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

module Storage.Beam.BusinessEvent where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.BusinessEvent as Domain
import Domain.Types.Vehicle.Variant (Variant)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common ()
import Lib.Utils ()
import Sequelize

data BusinessEventT f = BusinessEventT
  { id :: B.C f Text,
    driverId :: B.C f (Maybe Text),
    eventType :: B.C f Domain.EventType,
    timeStamp :: B.C f Time.UTCTime,
    bookingId :: B.C f (Maybe Text),
    whenPoolWasComputed :: B.C f (Maybe Domain.WhenPoolWasComputed),
    vehicleVariant :: B.C f (Maybe Variant),
    distance :: B.C f (Maybe Int),
    duration :: B.C f (Maybe Int),
    rideId :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table BusinessEventT where
  data PrimaryKey BusinessEventT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type BusinessEvent = BusinessEventT Identity

businessEventTMod :: BusinessEventT (B.FieldModification (B.TableField BusinessEventT))
businessEventTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      driverId = B.fieldNamed "driver_id",
      eventType = B.fieldNamed "event_type",
      timeStamp = B.fieldNamed "time_stamp",
      bookingId = B.fieldNamed "booking_id",
      whenPoolWasComputed = B.fieldNamed "when_pool_was_computed",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      distance = B.fieldNamed "distance",
      duration = B.fieldNamed "duration",
      rideId = B.fieldNamed "ride_id"
    }

$(enableKVPG ''BusinessEventT ['id] [])

$(mkTableInstances ''BusinessEventT "business_event" "atlas_driver_offer_bpp")
