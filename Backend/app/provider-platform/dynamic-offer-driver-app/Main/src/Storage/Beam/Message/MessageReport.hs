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

module Storage.Beam.Message.MessageReport where

import Data.Aeson
import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString)
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
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (..), fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.Message.MessageReport as Domain
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

instance FromField Domain.DeliveryStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.DeliveryStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.DeliveryStatus

instance FromBackendRow Postgres Domain.DeliveryStatus

instance IsString Domain.DeliveryStatus where
  fromString = show

fromFieldmessageDynamicFields ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Domain.MessageDynamicFieldsType
fromFieldmessageDynamicFields f mbValue = do
  value <- fromField f mbValue
  case A.fromJSON value of
    A.Success val -> pure val
    _ -> DPSF.returnError ConversionFailed f "Conversion failed."

instance FromField Domain.MessageDynamicFieldsType where
  fromField = fromFieldmessageDynamicFields

instance HasSqlValueSyntax be Value => HasSqlValueSyntax be Domain.MessageDynamicFieldsType where
  sqlValueSyntax = sqlValueSyntax . A.toJSON

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.MessageDynamicFieldsType

instance FromBackendRow Postgres Domain.MessageDynamicFieldsType

instance IsString Domain.MessageDynamicFieldsType where
  fromString = show

deriving stock instance Ord Domain.DeliveryStatus

data MessageReportT f = MessageReportT
  { messageId :: B.C f Text,
    driverId :: B.C f Text,
    deliveryStatus :: B.C f Domain.DeliveryStatus,
    readStatus :: B.C f Bool,
    likeStatus :: B.C f Bool,
    reply :: B.C f (Maybe Text),
    messageDynamicFields :: B.C f Domain.MessageDynamicFieldsType,
    updatedAt :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MessageReportT where
  data PrimaryKey MessageReportT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

instance ModelMeta MessageReportT where
  modelFieldModification = messageReportTMod
  modelTableName = "message_report"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type MessageReport = MessageReportT Identity

instance FromJSON MessageReport where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON MessageReport where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show MessageReport

messageReportTMod :: MessageReportT (B.FieldModification (B.TableField MessageReportT))
messageReportTMod =
  B.tableModification
    { messageId = B.fieldNamed "message_id",
      driverId = B.fieldNamed "driver_id",
      deliveryStatus = B.fieldNamed "delivery_status",
      readStatus = B.fieldNamed "read_status",
      likeStatus = B.fieldNamed "like_status",
      reply = B.fieldNamed "reply",
      messageDynamicFields = B.fieldNamed "message_dynamic_fields",
      updatedAt = B.fieldNamed "updated_at",
      createdAt = B.fieldNamed "created_at"
    }

defaultMessageReport :: MessageReport
defaultMessageReport =
  MessageReportT
    { messageId = "",
      driverId = "",
      deliveryStatus = "",
      readStatus = False,
      likeStatus = False,
      reply = Nothing,
      messageDynamicFields = "",
      updatedAt = defaultUTCDate,
      createdAt = defaultUTCDate
    }

instance Serialize MessageReport where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

messageReportToHSModifiers :: M.Map Text (A.Value -> A.Value)
messageReportToHSModifiers =
  M.empty

messageReportToPSModifiers :: M.Map Text (A.Value -> A.Value)
messageReportToPSModifiers =
  M.empty

$(enableKVPG ''MessageReportT ['driverId] [])
