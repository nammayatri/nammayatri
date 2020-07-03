{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Epass.Types.Storage.User where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Epass.Types.App
import qualified Epass.Utils.Defaults as Defaults
import EulerHS.Prelude
import Servant
import Web.HttpApiData

data Status = ACTIVE | INACTIVE
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Status where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Status where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToSchema Status

instance ToParamSchema Status

data Role
  = ADMIN
  | VALIDATOR
  | MANAGER
  | VIEWER
  | WARDLEVEL
  | DISTRICTLEVEL
  | CITYLEVEL
  | STATELEVEL
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, Enum, Bounded)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Role where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres Role

instance FromBackendRow Postgres Role where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToSchema Role

instance ToParamSchema Role

instance FromHttpApiData Role where
  parseUrlPiece = parseBoundedTextData
  parseQueryParam = parseBoundedTextData
  parseHeader = parseBoundedTextData . DT.decodeUtf8

data UserT f = User
  { _id :: B.C f UserId,
    _OrganizationId :: B.C f OrganizationId,
    _TenantOrganizationId :: B.C f (Maybe TenantOrganizationId),
    _name :: B.C f Text,
    _username :: B.C f (Maybe Text),
    _mobileNumber :: B.C f Text,
    _email :: B.C f (Maybe Text),
    _LocationId :: B.C f Text,
    _role :: B.C f Role,
    _verified :: B.C f Bool,
    _status :: B.C f Status,
    _info :: B.C f (Maybe Text),
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

type User = UserT Identity

type UserPrimaryKey = B.PrimaryKey UserT Identity

instance B.Table UserT where
  data PrimaryKey UserT f = UserPrimaryKey (B.C f UserId)
    deriving (Generic, B.Beamable)
  primaryKey = UserPrimaryKey . _id

instance Default User where
  def =
    User
      { _id = UserId Defaults.id,
        _OrganizationId = OrganizationId Defaults.orgId,
        _TenantOrganizationId = Nothing,
        _name = Defaults.user,
        _username = Just Defaults.user,
        _mobileNumber = "",
        _email = Just Defaults.email,
        _LocationId = Defaults.locId,
        _role = VIEWER,
        _verified = False,
        _status = ACTIVE,
        _info = Nothing,
        _createdAt = Defaults.localTime,
        _updatedAt = Defaults.localTime
      }

deriving instance Show User

deriving instance Eq User

instance ToSchema User

instance ToJSON User where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON User where
  parseJSON = genericParseJSON stripLensPrefixOptions

insertExpression user = insertExpressions [user]

insertExpressions users = B.insertValues users

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity UserT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _createdAt = "created_at",
        _updatedAt = "updated_at",
        _mobileNumber = "mobile_number",
        _LocationId = "location_id",
        _OrganizationId = "organization_id",
        _TenantOrganizationId = "tenant_organization_id"
      }
