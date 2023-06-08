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

module Storage.Beam.RegistrationToken where

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
import qualified Domain.Types.RegistrationToken as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
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

instance FromField Domain.Medium where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.Medium where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.Medium

instance FromBackendRow Postgres Domain.Medium

instance FromField Domain.LoginType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.LoginType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.LoginType

instance FromBackendRow Postgres Domain.LoginType

instance FromField Domain.RTEntityType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.RTEntityType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.RTEntityType

instance FromBackendRow Postgres Domain.RTEntityType

-- instance FromField RegToken where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be RegToken where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be RegToken

-- instance FromBackendRow Postgres RegToken

data RegistrationTokenT f = RegistrationTokenT
  { id :: B.C f Text,
    token :: B.C f RegToken,
    attempts :: B.C f Int,
    authMedium :: B.C f Domain.Medium,
    authType :: B.C f Domain.LoginType,
    authValueHash :: B.C f Text,
    verified :: B.C f Bool,
    authExpiry :: B.C f Int,
    tokenExpiry :: B.C f Int,
    entityId :: B.C f Text,
    merchantId :: B.C f Text,
    entityType :: B.C f Domain.RTEntityType,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime,
    info :: B.C f (Maybe Text),
    alternateNumberAttempts :: B.C f Int
  }
  deriving (Generic, B.Beamable)

instance B.Table RegistrationTokenT where
  data PrimaryKey RegistrationTokenT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta RegistrationTokenT where
  modelFieldModification = registrationTokenTMod
  modelTableName = "registration_token"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type RegistrationToken = RegistrationTokenT Identity

instance FromJSON RegistrationToken where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON RegistrationToken where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show RegistrationToken

deriving stock instance Ord Domain.Medium

deriving stock instance Ord Domain.LoginType

deriving stock instance Ord Domain.RTEntityType

instance IsString Domain.LoginType where
  fromString = show

instance IsString Domain.Medium where
  fromString = show

instance IsString Domain.RTEntityType where
  fromString = show

registrationTokenTMod :: RegistrationTokenT (B.FieldModification (B.TableField RegistrationTokenT))
registrationTokenTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      token = B.fieldNamed "token",
      attempts = B.fieldNamed "attempts",
      authMedium = B.fieldNamed "auth_medium",
      authType = B.fieldNamed "auth_type",
      authValueHash = B.fieldNamed "auth_value_hash",
      verified = B.fieldNamed "verified",
      authExpiry = B.fieldNamed "auth_expiry",
      tokenExpiry = B.fieldNamed "token_expiry",
      entityId = B.fieldNamed "entity_id",
      merchantId = B.fieldNamed "merchant_id",
      entityType = B.fieldNamed "entity_type",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at",
      info = B.fieldNamed "info",
      alternateNumberAttempts = B.fieldNamed "alternate_number_attempts"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

registrationTokenToHSModifiers :: M.Map Text (A.Value -> A.Value)
registrationTokenToHSModifiers =
  M.empty

registrationTokenToPSModifiers :: M.Map Text (A.Value -> A.Value)
registrationTokenToPSModifiers =
  M.empty

defaultRegistrationToken :: RegistrationToken
defaultRegistrationToken =
  RegistrationTokenT
    { id = "",
      token = "",
      attempts = 0,
      authMedium = "",
      authType = "",
      authValueHash = "",
      verified = False,
      authExpiry = 10,
      tokenExpiry = 10,
      entityId = "",
      merchantId = "",
      entityType = "",
      createdAt = defaultUTCDate,
      updatedAt = defaultUTCDate,
      info = Nothing,
      alternateNumberAttempts = 0
    }

instance Serialize RegistrationToken where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''RegistrationTokenT ['id] [])
