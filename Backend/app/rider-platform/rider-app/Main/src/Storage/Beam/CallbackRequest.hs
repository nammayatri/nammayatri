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

module Storage.Beam.CallbackRequest where

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
import qualified Domain.Types.CallbackRequest as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize

instance FromField Domain.CallbackRequestStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.CallbackRequestStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.CallbackRequestStatus

instance FromBackendRow Postgres Domain.CallbackRequestStatus

instance IsString Domain.CallbackRequestStatus where
  fromString = show

data CallbackRequestT f = CallbackRequestT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    customerName :: B.C f (Maybe Text),
    customerPhoneEncrypted :: B.C f Text,
    customerPhoneHash :: B.C f DbHash,
    customerMobileCountryCode :: B.C f Text,
    status :: B.C f Domain.CallbackRequestStatus,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CallbackRequestT where
  data PrimaryKey CallbackRequestT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta CallbackRequestT where
  modelFieldModification = callbackRequestTMod
  modelTableName = "callback_request"
  modelSchemaName = Just "atlas_app"

type CallbackRequest = CallbackRequestT Identity

instance FromJSON CallbackRequest where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON CallbackRequest where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show CallbackRequest

callbackRequestTMod :: CallbackRequestT (B.FieldModification (B.TableField CallbackRequestT))
callbackRequestTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      merchantId = B.fieldNamed "merchant_id",
      customerName = B.fieldNamed "customer_name",
      customerPhoneEncrypted = B.fieldNamed "customer_phone_encrypted",
      customerPhoneHash = B.fieldNamed "customer_phone_hash",
      customerMobileCountryCode = B.fieldNamed "customer_mobile_country_code",
      status = B.fieldNamed "status",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

defaultCallbackRequest :: CallbackRequest
defaultCallbackRequest =
  CallbackRequestT
    { id = "",
      merchantId = "",
      customerName = Nothing,
      customerPhoneEncrypted = "",
      customerPhoneHash = "",
      customerMobileCountryCode = "",
      status = "",
      createdAt = defaultUTCDate,
      updatedAt = defaultUTCDate
    }

instance Serialize CallbackRequest where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

callbackRequestToHSModifiers :: M.Map Text (A.Value -> A.Value)
callbackRequestToHSModifiers =
  M.empty

callbackRequestToPSModifiers :: M.Map Text (A.Value -> A.Value)
callbackRequestToPSModifiers =
  M.empty

$(enableKVPG ''CallbackRequestT ['id] [])
