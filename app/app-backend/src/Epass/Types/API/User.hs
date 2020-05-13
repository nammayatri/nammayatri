module Epass.Types.API.User where

import Beckn.Types.Storage.Person
import Data.Default
import Data.Swagger
import Data.Time.LocalTime
import Epass.Types.API.Common
import Epass.Types.App
import EulerHS.Prelude

data CreateReq = CreateReq
  { _name :: Text,
    _mobileNumber :: Text,
    _mobileCountryCode :: Maybe Text,
    _info :: Maybe Text,
    _role :: Role,
    _locationId :: Text,
    _organizationId :: OrganizationId
  }
  deriving (Show, Generic, ToJSON, ToSchema)

instance FromJSON CreateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

type CreateRes = UserInfo

data UpdateReq = UpdateReq
  { _name :: Maybe Text,
    _email :: Maybe Text,
    _role :: Maybe Role,
    _status :: Status
  }
  deriving (Show, Generic, ToJSON, ToSchema)

instance FromJSON UpdateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data UpdateRes = UpdateRes
  { _user :: Person
  }
  deriving (Show, Generic, FromJSON, ToSchema)

instance ToJSON UpdateRes where
  toJSON = genericToJSON stripLensPrefixOptions

data ListRes = ListRes
  { _users :: [Person]
  }
  deriving (Show, Generic, FromJSON, ToSchema)

instance ToJSON ListRes where
  toJSON = genericToJSON stripLensPrefixOptions

type GetRes = UserInfo
