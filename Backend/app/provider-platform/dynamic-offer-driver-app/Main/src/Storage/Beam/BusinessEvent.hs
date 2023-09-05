{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.BusinessEvent where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.BusinessEvent as Domain
import Domain.Types.Vehicle.Variant (Variant)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common ()
import Sequelize
import Tools.Beam.UtilsTH

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

$(enableKVPG ''BusinessEventT ['id] [])

$(mkTableInstances ''BusinessEventT "business_event")
