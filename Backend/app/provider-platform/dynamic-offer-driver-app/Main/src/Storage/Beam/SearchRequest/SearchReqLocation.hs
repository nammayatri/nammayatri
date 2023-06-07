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

module Storage.Beam.SearchRequest.SearchReqLocation where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.Utils
import Lib.UtilsTH
import Sequelize

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

data SearchReqLocationT f = SearchReqLocationT
  { id :: B.C f Text,
    lat :: B.C f Double,
    lon :: B.C f Double,
    street :: B.C f (Maybe Text),
    door :: B.C f (Maybe Text),
    city :: B.C f (Maybe Text),
    state :: B.C f (Maybe Text),
    country :: B.C f (Maybe Text),
    building :: B.C f (Maybe Text),
    full_address :: B.C f (Maybe Text),
    areaCode :: B.C f (Maybe Text),
    area :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchReqLocationT where
  data PrimaryKey SearchReqLocationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta SearchReqLocationT where
  modelFieldModification = searchReqLocationTMod
  modelTableName = "search_request_location"
  mkExprWithDefault _ = B.insertExpressions []
  modelSchemaName = Just "atlas_driver_offer_bpp"

type SearchReqLocation = SearchReqLocationT Identity

instance FromJSON SearchReqLocation where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON SearchReqLocation where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show SearchReqLocation

searchReqLocationTMod :: SearchReqLocationT (B.FieldModification (B.TableField SearchReqLocationT))
searchReqLocationTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      lat = B.fieldNamed "lat",
      lon = B.fieldNamed "lon",
      street = B.fieldNamed "street",
      door = B.fieldNamed "door",
      city = B.fieldNamed "city",
      state = B.fieldNamed "state",
      country = B.fieldNamed "country",
      building = B.fieldNamed "building",
      full_address = B.fieldNamed "full_address",
      areaCode = B.fieldNamed "area_code",
      area = B.fieldNamed "area",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

defaultSearchReqLocation :: SearchReqLocation
defaultSearchReqLocation =
  SearchReqLocationT
    { id = "",
      lat = 0.0,
      lon = 0.0,
      street = Nothing,
      door = Nothing,
      city = Nothing,
      state = Nothing,
      country = Nothing,
      building = Nothing,
      full_address = Nothing,
      areaCode = Nothing,
      area = Nothing,
      createdAt = defaultUTCDate,
      updatedAt = defaultUTCDate
    }

instance Serialize SearchReqLocation where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

searchReqLocationToHSModifiers :: M.Map Text (A.Value -> A.Value)
searchReqLocationToHSModifiers =
  M.empty

searchReqLocationToPSModifiers :: M.Map Text (A.Value -> A.Value)
searchReqLocationToPSModifiers =
  M.empty

$(enableKVPG ''SearchReqLocationT ['id] [])
