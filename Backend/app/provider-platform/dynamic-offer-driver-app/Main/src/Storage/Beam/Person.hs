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

module Storage.Beam.Person where

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
import qualified Domain.Types.Person as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Encryption (DbHash (..), Encrypted (..), EncryptedHashed (..))
import Kernel.External.FCM.Types (FCMRecipientToken (..))
import Kernel.External.Types (Language)
import Kernel.External.Whatsapp.Interface.Types (OptApiMethods (..))
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common (Centesimal)
import Kernel.Types.Common hiding (id)
import Kernel.Utils.Version
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Merchant (MerchantTId)

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

instance FromField Centesimal where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Centesimal where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centesimal

instance FromBackendRow Postgres Centesimal

instance FromField Language where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Language where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Language

instance FromBackendRow Postgres Language

instance FromField OptApiMethods where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be OptApiMethods where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be OptApiMethods

instance FromBackendRow Postgres OptApiMethods

instance FromField Domain.Gender where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.Gender where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.Gender

instance FromBackendRow Postgres Domain.Gender

instance FromField DbHash where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be DbHash where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DbHash

instance FromBackendRow Postgres DbHash

instance FromField Domain.Role where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.Role where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.Role

instance FromBackendRow Postgres Domain.Role

instance FromField FCMRecipientToken where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be FCMRecipientToken where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be FCMRecipientToken

instance FromBackendRow Postgres FCMRecipientToken

instance FromField Domain.IdentifierType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.IdentifierType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.IdentifierType

instance FromBackendRow Postgres Domain.IdentifierType

data PersonT f = PersonT
  { id :: B.C f Text,
    firstName :: B.C f Text,
    middleName :: B.C f (Maybe Text),
    lastName :: B.C f (Maybe Text),
    role :: B.C f Domain.Role,
    gender :: B.C f Domain.Gender,
    identifierType :: B.C f Domain.IdentifierType,
    email :: B.C f (Maybe Text),
    unencryptedMobileNumber :: B.C f (Maybe Text),
    mobileNumberEncrypted :: B.C f (Maybe Text),
    mobileNumberHash :: B.C f (Maybe DbHash),
    mobileCountryCode :: B.C f (Maybe Text),
    passwordHash :: B.C f (Maybe DbHash),
    identifier :: B.C f (Maybe Text),
    rating :: B.C f (Maybe Centesimal),
    isNew :: B.C f Bool,
    merchantId :: B.C f Text,
    deviceToken :: B.C f (Maybe FCMRecipientToken),
    language :: B.C f (Maybe Language),
    whatsappNotificationEnrollStatus :: B.C f (Maybe OptApiMethods),
    description :: B.C f (Maybe Text),
    alternateMobileNumberEncrypted :: B.C f (Maybe Text),
    unencryptedAlternateMobileNumber :: B.C f (Maybe Text),
    alternateMobileNumberHash :: B.C f (Maybe DbHash),
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime,
    bundleVersion :: B.C f (Maybe Text),
    clientVersion :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonT where
  data PrimaryKey PersonT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta PersonT where
  modelFieldModification = personTMod
  modelTableName = "person"
  mkExprWithDefault _ = B.insertExpressions []

type Person = PersonT Identity

instance FromJSON Person where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Person where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Person

deriving stock instance Ord Domain.Role

deriving stock instance Read FCMRecipientToken

deriving stock instance Ord Domain.Gender

deriving stock instance Ord Domain.IdentifierType

deriving stock instance Ord OptApiMethods

personTMod :: PersonT (B.FieldModification (B.TableField PersonT))
personTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      firstName = B.fieldNamed "first_name",
      middleName = B.fieldNamed "middle_name",
      lastName = B.fieldNamed "last_name",
      role = B.fieldNamed "role",
      gender = B.fieldNamed "gender",
      identifierType = B.fieldNamed "identifier_type",
      email = B.fieldNamed "email",
      unencryptedMobileNumber = B.fieldNamed "unencrypted_mobile_number",
      mobileNumberEncrypted = B.fieldNamed "mobile_number_encrypted",
      mobileNumberHash = B.fieldNamed "mobile_number_hash",
      mobileCountryCode = B.fieldNamed "mobile_country_code",
      passwordHash = B.fieldNamed "password_hash",
      identifier = B.fieldNamed "identifier",
      rating = B.fieldNamed "rating",
      isNew = B.fieldNamed "is_new",
      merchantId = B.fieldNamed "merchant_id",
      deviceToken = B.fieldNamed "device_token",
      language = B.fieldNamed "language",
      whatsappNotificationEnrollStatus = B.fieldNamed "whatsapp_notification_enroll_status",
      description = B.fieldNamed "description",
      alternateMobileNumberEncrypted = B.fieldNamed "alternate_mobile_number_encrypted",
      unencryptedAlternateMobileNumber = B.fieldNamed "unencrypted_alternate_mobile_number",
      alternateMobileNumberHash = B.fieldNamed "alternate_mobile_number_hash",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at",
      bundleVersion = B.fieldNamed "bundle_version",
      clientVersion = B.fieldNamed "client_version"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

personToHSModifiers :: M.Map Text (A.Value -> A.Value)
personToHSModifiers =
  M.fromList
    []

personToPSModifiers :: M.Map Text (A.Value -> A.Value)
personToPSModifiers =
  M.fromList
    []

$(enableKVPG ''PersonT ['id] [])
