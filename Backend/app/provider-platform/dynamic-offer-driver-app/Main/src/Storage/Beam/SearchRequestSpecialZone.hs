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

module Storage.Beam.SearchRequestSpecialZone where

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
import qualified Domain.Types.SearchRequestSpecialZone as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Kernel.Utils.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.SearchRequest.SearchReqLocation (SearchReqLocationT, SearchReqLocationTId, mkDomainSearchReqLocation, mkTabularSearchReqLocation)
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

instance FromField Meters where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

instance FromBackendRow Postgres Meters

instance FromField Seconds where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Seconds where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

instance FromBackendRow Postgres Seconds

data SearchRequestSpecialZoneT f = SearchRequestSpecialZoneT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    messageId :: B.C f Text,
    startTime :: B.C f Time.LocalTime,
    validTill :: B.C f Time.LocalTime,
    providerId :: B.C f Text,
    fromLocationId :: B.C f Text,
    toLocationId :: B.C f Text,
    bapId :: B.C f Text,
    bapUri :: B.C f Text,
    estimatedDistance :: B.C f Meters,
    estimatedDuration :: B.C f Seconds,
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance IsString Seconds where
  fromString = show

instance IsString Meters where
  fromString = show

instance B.Table SearchRequestSpecialZoneT where
  data PrimaryKey SearchRequestSpecialZoneT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta SearchRequestSpecialZoneT where
  modelFieldModification = searchRequestSpecialZoneTMod
  modelTableName = "search_request_special_zone"
  mkExprWithDefault _ = B.insertExpressions []

type SearchRequestSpecialZone = SearchRequestSpecialZoneT Identity

instance FromJSON SearchRequestSpecialZone where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON SearchRequestSpecialZone where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show SearchRequestSpecialZone

searchRequestSpecialZoneTMod :: SearchRequestSpecialZoneT (B.FieldModification (B.TableField SearchRequestSpecialZoneT))
searchRequestSpecialZoneTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      transactionId = B.fieldNamed "transaction_id",
      messageId = B.fieldNamed "message_id",
      startTime = B.fieldNamed "start_time",
      validTill = B.fieldNamed "valid_till",
      providerId = B.fieldNamed "provider_id",
      fromLocationId = B.fieldNamed "from_location_id",
      toLocationId = B.fieldNamed "to_location_id",
      bapId = B.fieldNamed "bap_id",
      bapUri = B.fieldNamed "bap_uri",
      estimatedDistance = B.fieldNamed "estimated_distance",
      estimatedDuration = B.fieldNamed "estimated_duration",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

searchRequestSpecialZoneToHSModifiers :: M.Map Text (A.Value -> A.Value)
searchRequestSpecialZoneToHSModifiers =
  M.fromList
    []

searchRequestSpecialZoneToPSModifiers :: M.Map Text (A.Value -> A.Value)
searchRequestSpecialZoneToPSModifiers =
  M.fromList
    []

instance Serialize SearchRequestSpecialZone where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''SearchRequestSpecialZoneT ['id] [])
