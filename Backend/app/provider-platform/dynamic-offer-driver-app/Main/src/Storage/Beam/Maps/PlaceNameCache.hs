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

module Storage.Beam.Maps.PlaceNameCache where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
    ResultError (UnexpectedNull),
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.Maps.PlaceNameCache as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.Utils
import Lib.UtilsTH
import Sequelize

fromFieldAddressResp ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion [Domain.AddressResp]
fromFieldAddressResp f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> V.toList <$> fromField f mbValue

instance FromField Domain.AddressResp where
  fromField = fromFieldEnum

instance FromField [Domain.AddressResp] where
  fromField = fromFieldAddressResp

instance HasSqlValueSyntax be String => HasSqlValueSyntax be [Domain.AddressResp] where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Domain.AddressResp]

instance FromBackendRow Postgres [Domain.AddressResp]

data PlaceNameCacheT f = PlaceNameCacheT
  { id :: B.C f Text,
    formattedAddress :: B.C f (Maybe Text),
    plusCode :: B.C f (Maybe Text),
    lat :: B.C f Double,
    lon :: B.C f Double,
    placeId :: B.C f (Maybe Text),
    geoHash :: B.C f (Maybe Text),
    addressComponents :: B.C f [Domain.AddressResp],
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PlaceNameCacheT where
  data PrimaryKey PlaceNameCacheT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta PlaceNameCacheT where
  modelFieldModification = placeNameCacheTMod
  modelTableName = "place_name_cache"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type PlaceNameCache = PlaceNameCacheT Identity

instance FromJSON PlaceNameCache where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON PlaceNameCache where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show PlaceNameCache

placeNameCacheTMod :: PlaceNameCacheT (B.FieldModification (B.TableField PlaceNameCacheT))
placeNameCacheTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      formattedAddress = B.fieldNamed "formatted_address",
      plusCode = B.fieldNamed "plus_code",
      lat = B.fieldNamed "lat",
      lon = B.fieldNamed "lon",
      placeId = B.fieldNamed "place_id",
      geoHash = B.fieldNamed "geo_hash",
      addressComponents = B.fieldNamed "address_components",
      createdAt = B.fieldNamed "created_at"
    }

instance Serialize PlaceNameCache where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

placeNameCacheToHSModifiers :: M.Map Text (A.Value -> A.Value)
placeNameCacheToHSModifiers =
  M.empty

placeNameCacheToPSModifiers :: M.Map Text (A.Value -> A.Value)
placeNameCacheToPSModifiers =
  M.empty

$(enableKVPG ''PlaceNameCacheT ['id] [])
