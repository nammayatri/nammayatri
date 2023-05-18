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

module Storage.Beam.DriverLocation where

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
import qualified Domain.Types.DriverLocation as Domain
import Domain.Types.Person (Person)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Person (PersonTId)

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

instance FromField Point where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Point where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Point

instance FromBackendRow Postgres Point

data DriverLocationT f = DriverLocationT
  { driverId :: B.C f Text,
    lat :: B.C f Double,
    lon :: B.C f Double,
    point :: B.C f Point,
    coordinatesCalculatedAt :: B.C f Time.LocalTime,
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverLocationT where
  data PrimaryKey DriverLocationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta DriverLocationT where
  modelFieldModification = driverLocationTMod
  modelTableName = "driver_location"
  mkExprWithDefault _ = B.insertExpressions []

type DriverLocation = DriverLocationT Identity

instance FromJSON DriverLocation where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverLocation where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverLocation

driverLocationTMod :: DriverLocationT (B.FieldModification (B.TableField DriverLocationT))
driverLocationTMod =
  B.tableModification
    { driverId = B.fieldNamed "driver_id",
      lat = B.fieldNamed "lat",
      lon = B.fieldNamed "lon",
      point = B.fieldNamed "point",
      coordinatesCalculatedAt = B.fieldNamed "coordinates_calculated_at",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverLocationToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverLocationToHSModifiers =
  M.fromList
    []

driverLocationToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverLocationToPSModifiers =
  M.fromList
    []

instance Serialize DriverLocation where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''DriverLocationT ['driverId] [])
