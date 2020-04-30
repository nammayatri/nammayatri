{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.User where

import           Beckn.Types.App
import qualified Beckn.Utils.Defaults      as Defaults
import           Data.Aeson
import           Data.Default
import           Data.Swagger
import qualified Data.Text                 as T
import           Data.Time
import qualified Database.Beam             as B
import           Database.Beam.Backend.SQL
import           Database.Beam.MySQL
import           EulerHS.Prelude

data Status = ACTIVE | INACTIVE
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Status where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL Status where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data Role = ADMIN | MANAGER | VIEWER
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Role where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL Role where
  fromBackendRow = read . T.unpack <$> fromBackendRow


data UserT f =
  User
    { _id                   :: B.C f UserId
    , _OrganizationId       :: B.C f OrganizationId
    , _TenantOrganizationId :: B.C f (Maybe TenantOrganizationId)
    , _name                 :: B.C f Text
    , _username             :: B.C f Text
    , _mobileNumber         :: B.C f Text
    , _email                :: B.C f Text
    , _role                 :: B.C f Role
    , _verified             :: B.C f Bool
    , _status               :: B.C f Status
    , _info                 :: B.C f (Maybe Text)
    , _createdAt            :: B.C f LocalTime
    , _updatedAt            :: B.C f LocalTime
    }
  deriving (Generic, B.Beamable)

type User = UserT Identity

type UserPrimaryKey = B.PrimaryKey UserT Identity

instance B.Table UserT where
  data PrimaryKey UserT f = UserPrimaryKey (B.C f UserId)
                               deriving (Generic, B.Beamable)
  primaryKey = UserPrimaryKey . _id

instance Default User where
  def = User
    { _id             = UserId Defaults.id
    , _OrganizationId = OrganizationId Defaults.orgId
    , _TenantOrganizationId = Nothing
    , _name           = Defaults.user
    , _username       = Defaults.user
    , _mobileNumber       = ""
    , _email          = Defaults.email
    , _role           = VIEWER
    , _verified       = False
    , _status         = ACTIVE
    , _info           = Nothing
    , _createdAt      = Defaults.localTime
    , _updatedAt      = Defaults.localTime
    }

deriving instance Show User

deriving instance Eq User

instance ToJSON User where
  toJSON = genericToJSON stripLensPrefixOptions

deriving instance FromJSON User

instance ToSchema User

insertExpression user = insertExpressions [user]

insertExpressions users = B.insertValues users

fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity UserT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _createdAt = "created_at"
      , _updatedAt = "updated_at"
      , _mobileNumber = "mobile_number"
      , _OrganizationId = "organization_id"
      , _TenantOrganizationId = "tenant_organization_id"
      }


