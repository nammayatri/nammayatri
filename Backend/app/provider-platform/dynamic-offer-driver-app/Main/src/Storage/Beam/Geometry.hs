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

module Storage.Beam.Geometry where

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
import qualified Domain.Types.Geometry as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize

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

data GeometryT f = GeometryT
  { id :: B.C f Text,
    region :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table GeometryT where
  data PrimaryKey GeometryT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta GeometryT where
  modelFieldModification = geometryTMod
  modelTableName = "geometry"
  mkExprWithDefault _ = B.insertExpressions []

type Geometry = GeometryT Identity

instance FromJSON Geometry where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Geometry where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Geometry

geometryTMod :: GeometryT (B.FieldModification (B.TableField GeometryT))
geometryTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      region = B.fieldNamed "region"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

geometryToHSModifiers :: M.Map Text (A.Value -> A.Value)
geometryToHSModifiers =
  M.fromList
    []

geometryToPSModifiers :: M.Map Text (A.Value -> A.Value)
geometryToPSModifiers =
  M.fromList
    []

defaultGeometry :: Geometry
defaultGeometry =
  GeometryT
    { id = "",
      region = ""
    }

instance Serialize Geometry where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''GeometryT ['id] [])
