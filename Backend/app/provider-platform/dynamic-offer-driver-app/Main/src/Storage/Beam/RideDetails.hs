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
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.RideDetails where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Database.Beam.Schema.Tables as BST
import qualified Domain.Types.Vehicle as SV
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.PIIEncryption
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Sequelize

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

instance ModelMeta RideDetailsT where
  modelFieldModification = rideDetailsTMod
  modelTableName = "ride_details"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type RideDetails = RideDetailsT Identity

rideDetailsTable :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RideDetailsT)
rideDetailsTable =
  BST.setEntitySchema (Just "atlas_driver_offer_bpp")
    <> B.setEntityName "ride_details"
    <> B.modifyTableFields rideDetailsTMod

instance FromJSON RideDetails where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON RideDetails where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show RideDetails

rideDetailsTMod :: RideDetailsT (B.FieldModification (B.TableField RideDetailsT))
rideDetailsTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      driverName = B.fieldNamed "driver_name",
      driverNumberEncrypted = B.fieldNamed "driver_number_encrypted",
      driverNumberHash = B.fieldNamed "driver_number_hash",
      driverCountryCode = B.fieldNamed "driver_country_code",
      vehicleNumber = B.fieldNamed "vehicle_number",
      vehicleColor = B.fieldNamed "vehicle_color",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      vehicleModel = B.fieldNamed "vehicle_model",
      vehicleClass = B.fieldNamed "vehicle_class"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

rideDetailsToHSModifiers :: M.Map Text (A.Value -> A.Value)
rideDetailsToHSModifiers =
  M.empty

rideDetailsToPSModifiers :: M.Map Text (A.Value -> A.Value)
rideDetailsToPSModifiers =
  M.empty

instance Serialize RideDetails where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''RideDetailsT ['id] [])
