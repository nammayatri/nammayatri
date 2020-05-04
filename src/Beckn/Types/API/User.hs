module Beckn.Types.API.User where

import           Beckn.Types.App
import           Beckn.Types.API.Common
import           Beckn.Types.Storage.User
import           Data.Default
import           Data.Swagger
import           Data.Time.LocalTime
import           EulerHS.Prelude

data CreateReq =
  CreateReq
  { _name                 :: Text
  , _mobileNumber         :: Text
  , _info                 :: Maybe Text
  , _role                 :: Role
  , _LocationId           :: Text
  , _OrganizationId       :: OrganizationId
  , _TenantOrganizationId :: Maybe TenantOrganizationId
  }
  deriving (Show, Generic, ToJSON, ToSchema)


instance FromJSON CreateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions


type CreateRes = UserInfo

data UpdateReq =
  UpdateReq
  { _name   :: Maybe Text
  , _role   :: Maybe Role
  , _status :: Status
  }
  deriving (Show, Generic, ToJSON, ToSchema)

instance FromJSON UpdateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data UpdateRes =
  UpdateRes
  { _user :: User
  }
  deriving (Show, Generic, FromJSON, Default, ToSchema)

instance ToJSON UpdateRes where
  toJSON = genericToJSON stripLensPrefixOptions


data ListRes =
  ListRes
  { _users :: [User]
  }
  deriving (Show, Generic, FromJSON, Default, ToSchema)

instance ToJSON ListRes where
  toJSON = genericToJSON stripLensPrefixOptions

type GetRes = UserInfo
