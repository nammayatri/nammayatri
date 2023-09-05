{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.RideDetails where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Vehicle as SV
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Encryption
import Kernel.Prelude hiding (Generic)
import Sequelize
import Tools.Beam.UtilsTH

data RideDetailsT f = RideDetailsT
  { id :: B.C f Text,
    driverName :: B.C f Text,
    driverNumberEncrypted :: B.C f (Maybe Text),
    driverNumberHash :: B.C f (Maybe DbHash),
    driverCountryCode :: B.C f (Maybe Text),
    vehicleNumber :: B.C f Text,
    vehicleColor :: B.C f (Maybe Text),
    vehicleVariant :: B.C f (Maybe SV.Variant),
    vehicleModel :: B.C f (Maybe Text),
    vehicleClass :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table RideDetailsT where
  data PrimaryKey RideDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type RideDetails = RideDetailsT Identity

$(enableKVPG ''RideDetailsT ['id] [])

$(mkTableInstances ''RideDetailsT "ride_details")
