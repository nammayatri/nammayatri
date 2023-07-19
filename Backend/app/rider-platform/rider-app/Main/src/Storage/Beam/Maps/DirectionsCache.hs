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

module Storage.Beam.Maps.DirectionsCache where

import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Maps (BoundingBoxWithoutCRS (..), LatLong (..), PointXY (..), PointXYZ (..), PointXYZM (..), RouteInfo (..))
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.UtilsTH
import Sequelize

instance FromField RouteInfo where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RouteInfo where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be RouteInfo

instance FromBackendRow Postgres RouteInfo

instance IsString RouteInfo where
  fromString = show

deriving stock instance Eq RouteInfo

deriving stock instance Read RouteInfo

deriving stock instance Ord RouteInfo

deriving stock instance Ord LatLong

deriving stock instance Ord BoundingBoxWithoutCRS

deriving stock instance Ord PointXY

deriving stock instance Ord PointXYZ

deriving stock instance Ord PointXYZM

deriving stock instance Read LatLong

deriving stock instance Read BoundingBoxWithoutCRS

deriving stock instance Read PointXY

deriving stock instance Read PointXYZ

deriving stock instance Read PointXYZM

data DirectionsCacheT f = DirectionsCacheT
  { id :: B.C f Text,
    originHash :: B.C f Text,
    destHash :: B.C f Text,
    slot :: B.C f Int,
    response :: B.C f RouteInfo,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DirectionsCacheT where
  data PrimaryKey DirectionsCacheT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta DirectionsCacheT where
  modelFieldModification = directionsCacheTMod
  modelTableName = "directions_cache"
  modelSchemaName = Just "atlas_app"

type DirectionsCache = DirectionsCacheT Identity

instance FromJSON DirectionsCache where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DirectionsCache where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DirectionsCache

directionsCacheTMod :: DirectionsCacheT (B.FieldModification (B.TableField DirectionsCacheT))
directionsCacheTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      originHash = B.fieldNamed "origin_hash",
      destHash = B.fieldNamed "dest_hash",
      slot = B.fieldNamed "slot",
      response = B.fieldNamed "response",
      createdAt = B.fieldNamed "created_at"
    }

instance Serialize DirectionsCache where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

directionsCacheToHSModifiers :: M.Map Text (A.Value -> A.Value)
directionsCacheToHSModifiers =
  M.empty

directionsCacheToPSModifiers :: M.Map Text (A.Value -> A.Value)
directionsCacheToPSModifiers =
  M.empty

$(enableKVPG ''DirectionsCacheT ['id] [])
