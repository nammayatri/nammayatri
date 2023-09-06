{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Notification where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import qualified Database.Beam.Schema.Tables as BST
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Payment.Juspay.Types as Payment
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

instance FromField Payment.NotificationStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Payment.NotificationStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Payment.NotificationStatus

instance FromBackendRow Postgres Payment.NotificationStatus

deriving instance Ord Payment.NotificationStatus

instance IsString Payment.NotificationStatus where
  fromString = show

data NotificationT f = NotificationT
  { id :: B.C f Text,
    shortId :: B.C f Text,
    sourceAmount :: B.C f HighPrecMoney,
    mandateId :: B.C f Text,
    driverFeeId :: B.C f Text,
    txnDate :: B.C f UTCTime,
    juspayProvidedId :: B.C f Text,
    providerName :: B.C f Text,
    notificationType :: B.C f Text,
    description :: B.C f Text,
    status :: B.C f Payment.NotificationStatus,
    dateCreated :: B.C f UTCTime,
    lastUpdated :: B.C f UTCTime,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table NotificationT where
  data PrimaryKey NotificationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta NotificationT where
  modelFieldModification = notificationTMod
  modelTableName = "notification"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type Notification = NotificationT Identity

notificationTable :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity NotificationT)
notificationTable =
  BST.setEntitySchema (Just "atlas_driver_offer_bpp")
    <> B.setEntityName "notification"
    <> B.modifyTableFields notificationTMod

deriving stock instance Show Notification

instance FromJSON Notification where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Notification where
  toJSON = A.genericToJSON A.defaultOptions

notificationTMod :: NotificationT (B.FieldModification (B.TableField NotificationT))
notificationTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      sourceAmount = B.fieldNamed "source_amount",
      txnDate = B.fieldNamed "txn_date",
      mandateId = B.fieldNamed "mandate_id",
      driverFeeId = B.fieldNamed "driver_fee_id",
      juspayProvidedId = B.fieldNamed "juspay_provided_id",
      providerName = B.fieldNamed "provider_name",
      notificationType = B.fieldNamed "notification_type",
      description = B.fieldNamed "description",
      status = B.fieldNamed "status",
      dateCreated = B.fieldNamed "date_created",
      lastUpdated = B.fieldNamed "date_updated",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

notificationToHSModifiers :: M.Map Text (A.Value -> A.Value)
notificationToHSModifiers =
  M.empty

notificationToPSModifiers :: M.Map Text (A.Value -> A.Value)
notificationToPSModifiers =
  M.empty

instance Serialize Notification where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''NotificationT ['id] []) -- DON'T Enable for KV
