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
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Database.Beam.Schema.Tables as BST
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.PIIEncryption
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

data GeometryT f = GeometryT
  { id :: B.C f Text,
    region :: B.C f Text
  }
  deriving (Generic, B.Beamable)

geometryTable :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity GeometryT)
geometryTable =
  BST.setEntitySchema (Just "atlas_driver_offer_bpp")
    <> B.setEntityName "geometry"
    <> B.modifyTableFields geometryTMod

instance B.Table GeometryT where
  data PrimaryKey GeometryT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta GeometryT where
  modelFieldModification = geometryTMod
  modelTableName = "geometry"
  modelSchemaName = Just "atlas_driver_offer_bpp"

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
  M.empty

geometryToPSModifiers :: M.Map Text (A.Value -> A.Value)
geometryToPSModifiers =
  M.empty

instance Serialize Geometry where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''GeometryT ['id] [])
