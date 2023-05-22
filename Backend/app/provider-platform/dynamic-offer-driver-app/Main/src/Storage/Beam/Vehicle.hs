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

module Storage.Beam.Vehicle where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
    ResultError (ConversionFailed, UnexpectedNull),
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.Person as DPers
import qualified Domain.Types.Vehicle as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import qualified Storage.Tabular.Merchant as TM
import Storage.Tabular.Person (PersonTId)

-- fromFieldEnum ::
--   (Typeable a, Read a) =>
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion a
-- fromFieldEnum f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just value' ->
--     case (readMaybe (unpackChars value')) of
--       Just val -> pure val
--       _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

instance FromField Variant.Variant where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Variant.Variant where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Variant.Variant

instance FromBackendRow Postgres Variant.Variant

instance FromField Domain.RegistrationCategory where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.RegistrationCategory where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.RegistrationCategory

instance FromBackendRow Postgres Domain.RegistrationCategory

instance FromField Domain.Category where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.Category where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.Category

instance FromBackendRow Postgres Domain.Category

instance IsString Domain.RegistrationCategory where
  fromString = show

instance IsString Domain.Category where
  fromString = show

instance IsString Variant.Variant where
  fromString = show

data VehicleT f = VehicleT
  { driverId :: B.C f Text,
    merchantId :: B.C f Text,
    variant :: B.C f Variant.Variant,
    model :: B.C f Text,
    color :: B.C f Text,
    registrationNo :: B.C f Text,
    capacity :: B.C f (Maybe Int),
    category :: B.C f (Maybe Domain.Category),
    make :: B.C f (Maybe Text),
    size :: B.C f (Maybe Text),
    energyType :: B.C f (Maybe Text),
    registrationCategory :: B.C f (Maybe Domain.RegistrationCategory),
    vehicleClass :: B.C f Text,
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleT where
  data PrimaryKey VehicleT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

instance ModelMeta VehicleT where
  modelFieldModification = vehicleTMod
  modelTableName = "vehicle"
  mkExprWithDefault _ = B.insertExpressions []

type Vehicle = VehicleT Identity

instance FromJSON Vehicle where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Vehicle where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Vehicle

deriving stock instance Ord Domain.Category

deriving stock instance Ord Domain.RegistrationCategory

vehicleTMod :: VehicleT (B.FieldModification (B.TableField VehicleT))
vehicleTMod =
  B.tableModification
    { driverId = B.fieldNamed "driver_id",
      merchantId = B.fieldNamed "merchant_id",
      variant = B.fieldNamed "variant",
      model = B.fieldNamed "model",
      color = B.fieldNamed "color",
      registrationNo = B.fieldNamed "registration_no",
      capacity = B.fieldNamed "capacity",
      category = B.fieldNamed "category",
      make = B.fieldNamed "make",
      size = B.fieldNamed "size",
      energyType = B.fieldNamed "energy_type",
      registrationCategory = B.fieldNamed "registration_category",
      vehicleClass = B.fieldNamed "vehicle_class",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

vehicleToHSModifiers :: M.Map Text (A.Value -> A.Value)
vehicleToHSModifiers =
  M.fromList
    []

vehicleToPSModifiers :: M.Map Text (A.Value -> A.Value)
vehicleToPSModifiers =
  M.fromList
    []

defaultVehicle :: Vehicle
defaultVehicle =
  VehicleT
    { driverId = "",
      merchantId = "",
      variant = "",
      model = "",
      color = "",
      registrationNo = "",
      capacity = Nothing,
      category = Nothing,
      make = Nothing,
      size = Nothing,
      energyType = Nothing,
      registrationCategory = Nothing,
      vehicleClass = "",
      createdAt = defaultDate,
      updatedAt = defaultDate
    }

instance Serialize Vehicle where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''VehicleT ['driverId] [])
