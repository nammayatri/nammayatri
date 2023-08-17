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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Maps.DirectionsCache where

import Data.Aeson
import qualified Data.Aeson as A
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Serialize
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import Debug.Trace as T
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Maps (BoundingBoxWithoutCRS (..), LatLong (..), PointXY (..), PointXYZ (..), PointXYZM (..), RouteInfo (..))
import Kernel.Prelude hiding (Generic)
import Sequelize

instance FromField RouteInfo where
  fromField f mbValue = case mbValue of
    Nothing -> DPSF.returnError UnexpectedNull f mempty
    Just value' ->
      case T.trace ("text val" <> show value') $ A.eitherDecode $ fromStrict value' of
        Right jsonVal -> case T.trace ("text val" <> show value') $ A.eitherDecode (fromStrict $ TE.encodeUtf8 jsonVal) of
          Right val -> pure val
          _ -> DPSF.returnError ConversionFailed f ("Could not 'read'" <> show value')
        _ -> DPSF.returnError ConversionFailed f ("Could not 'read'" <> show value')

instance FromBackendRow Postgres RouteInfo

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be RouteInfo where
  sqlValueSyntax = sqlValueSyntax . (stringify . A.String . stringify . A.toJSON)
    where
      stringify = TE.decodeUtf8 . toStrict . A.encode

instance BeamSqlBackend be => B.HasSqlEqualityCheck be RouteInfo

-- instance FromBackendRow Postgres RouteInfo

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

type DirectionsCache = DirectionsCacheT Identity

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

$(enableKVPG ''DirectionsCacheT ['id] [['originHash], ['destHash]])

$(mkTableInstances ''DirectionsCacheT "directions_cache" "atlas_app")
