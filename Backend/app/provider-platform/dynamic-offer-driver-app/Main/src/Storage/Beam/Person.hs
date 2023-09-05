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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Person where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Person as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption (DbHash (..))
import Kernel.External.Notification.FCM.Types (FCMRecipientToken (..))
import Kernel.External.Types (Language)
import Kernel.External.Whatsapp.Interface.Types (OptApiMethods (..))
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

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

instance IsString Domain.Gender where
  fromString = show

instance FromField Domain.Role where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.Role where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.Role

instance FromBackendRow Postgres Domain.Role

instance IsString Domain.Role where
  fromString = show

deriving newtype instance FromField FCMRecipientToken

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be FCMRecipientToken where
  sqlValueSyntax = sqlValueSyntax . getFCMRecipientToken

instance BeamSqlBackend be => B.HasSqlEqualityCheck be FCMRecipientToken

instance FromBackendRow Postgres FCMRecipientToken

instance FromField Domain.IdentifierType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.IdentifierType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.IdentifierType

instance FromBackendRow Postgres Domain.IdentifierType

instance IsString Domain.IdentifierType where
  fromString = show

data PersonT f = PersonT
  { id :: B.C f Text,
    firstName :: B.C f Text,
    middleName :: B.C f (Maybe Text),
    lastName :: B.C f (Maybe Text),
    role :: B.C f Domain.Role,
    gender :: B.C f Domain.Gender,
    hometown :: B.C f (Maybe Text),
    languagesSpoken :: B.C f (Maybe [Text]),
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
    onboardedFromDashboard :: B.C f Bool,
    merchantId :: B.C f Text,
    deviceToken :: B.C f (Maybe FCMRecipientToken),
    language :: B.C f (Maybe Language),
    whatsappNotificationEnrollStatus :: B.C f (Maybe OptApiMethods),
    description :: B.C f (Maybe Text),
    alternateMobileNumberEncrypted :: B.C f (Maybe Text),
    unencryptedAlternateMobileNumber :: B.C f (Maybe Text),
    alternateMobileNumberHash :: B.C f (Maybe DbHash),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime,
    bundleVersion :: B.C f (Maybe Text),
    clientVersion :: B.C f (Maybe Text),
    faceImageId :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonT where
  data PrimaryKey PersonT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Person = PersonT Identity

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
      hometown = B.fieldNamed "hometown",
      languagesSpoken = B.fieldNamed "languages_spoken",
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
      onboardedFromDashboard = B.fieldNamed "onboarded_from_dashboard",
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
      clientVersion = B.fieldNamed "client_version",
      faceImageId = B.fieldNamed "face_image_id"
    }

$(enableKVPG ''PersonT ['id] [['mobileNumberHash]]) -- DON'T Enable for KV

$(mkTableInstances ''PersonT "person" "atlas_driver_offer_bpp")
