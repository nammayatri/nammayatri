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

module Storage.Beam.SearchRequest where

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
import qualified Domain.Types.SearchRequest as Domain
import qualified Domain.Types.Vehicle.Variant as Variant (Variant)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Kernel.Utils.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Estimate (EstimateTId)
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

instance FromField Variant.Variant where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Variant.Variant where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Variant.Variant

instance FromBackendRow Postgres Variant.Variant

instance FromField Seconds where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Seconds where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

instance FromBackendRow Postgres Seconds

instance FromField Domain.SearchRequestStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.SearchRequestStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.SearchRequestStatus

instance FromBackendRow Postgres Domain.SearchRequestStatus

instance FromField Meters where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

instance FromBackendRow Postgres Meters

instance FromField Money where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Money where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Money

instance FromBackendRow Postgres Money

data SearchRequestT f = SearchRequestT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    messageId :: B.C f Text,
    estimateId :: B.C f Text,
    startTime :: B.C f Time.LocalTime,
    validTill :: B.C f Time.LocalTime,
    providerId :: B.C f Text,
    fromLocationId :: B.C f Text,
    toLocationId :: B.C f Text,
    bapId :: B.C f Text,
    bapUri :: B.C f Text,
    estimatedDistance :: B.C f Meters,
    estimatedDuration :: B.C f Seconds,
    customerExtraFee :: B.C f (Maybe Money),
    device :: B.C f (Maybe Text),
    status :: B.C f Domain.SearchRequestStatus,
    vehicleVariant :: B.C f Variant.Variant,
    searchRepeatCounter :: B.C f Int,
    autoAssignEnabled :: B.C f Bool,
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance IsString Domain.SearchRequestStatus where
  fromString = show

instance IsString Variant.Variant where
  fromString = show

instance IsString Meters where
  fromString = show

instance IsString Seconds where
  fromString = show

instance B.Table SearchRequestT where
  data PrimaryKey SearchRequestT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta SearchRequestT where
  modelFieldModification = searchRequestTMod
  modelTableName = "search_request"
  mkExprWithDefault _ = B.insertExpressions []

type SearchRequest = SearchRequestT Identity

instance FromJSON SearchRequest where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON SearchRequest where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show SearchRequest

deriving stock instance Read Money

searchRequestTMod :: SearchRequestT (B.FieldModification (B.TableField SearchRequestT))
searchRequestTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      transactionId = B.fieldNamed "transaction_id",
      messageId = B.fieldNamed "message_id",
      estimateId = B.fieldNamed "estimate_id",
      startTime = B.fieldNamed "start_time",
      validTill = B.fieldNamed "valid_till",
      providerId = B.fieldNamed "provider_id",
      fromLocationId = B.fieldNamed "from_location_id",
      toLocationId = B.fieldNamed "to_location_id",
      bapId = B.fieldNamed "bap_id",
      bapUri = B.fieldNamed "bap_uri",
      estimatedDistance = B.fieldNamed "estimated_distance",
      estimatedDuration = B.fieldNamed "estimated_duration",
      customerExtraFee = B.fieldNamed "customer_extra_fee",
      device = B.fieldNamed "device",
      status = B.fieldNamed "status",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      searchRepeatCounter = B.fieldNamed "search_repeat_counter",
      autoAssignEnabled = B.fieldNamed "auto_assign_enabled",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

searchRequestToHSModifiers :: M.Map Text (A.Value -> A.Value)
searchRequestToHSModifiers =
  M.fromList
    []

searchRequestToPSModifiers :: M.Map Text (A.Value -> A.Value)
searchRequestToPSModifiers =
  M.fromList
    []

instance Serialize SearchRequest where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''SearchRequestT ['id] [])
