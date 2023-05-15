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
import qualified Domain.Types.Ride as SR
import qualified Domain.Types.RideDetails as Domain
import qualified Domain.Types.Vehicle as SV
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Encryption
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Ride (RideTId)
import Storage.Tabular.Vehicle ()

fromFieldEnum ::
  (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldEnum f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' ->
    case (readMaybe (unpackChars value')) of
      Just val -> pure val
      _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

instance FromField SV.Variant where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be SV.Variant where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be SV.Variant

instance FromBackendRow Postgres SV.Variant

instance FromField DbHash where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be DbHash where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DbHash

instance FromBackendRow Postgres DbHash

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
  mkExprWithDefault _ = B.insertExpressions []

type RideDetails = RideDetailsT Identity

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
  M.fromList
    []

rideDetailsToPSModifiers :: M.Map Text (A.Value -> A.Value)
rideDetailsToPSModifiers =
  M.fromList
    []

$(enableKVPG ''RideDetailsT ['id] [])
