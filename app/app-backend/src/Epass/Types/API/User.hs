module Epass.Types.API.User where

import Beckn.Types.Storage.Person
import Data.Default
import Data.Swagger
import Data.Time.LocalTime
import Epass.Types.API.Common
import Epass.Types.App
import EulerHS.Prelude

data UpdateReq = UpdateReq
  { _firstName :: Maybe Text,
    _middleName :: Maybe Text,
    _lastName :: Maybe Text,
    _fullName :: Maybe Text,
    _gender :: Maybe Gender,
    _email :: Maybe Text,
    _organizationId :: Maybe Text,
    _locationId :: Maybe Text,
    _description :: Maybe Text,
    _status :: Maybe Status
  }
  deriving (Show, Generic, ToJSON, ToSchema)

instance FromJSON UpdateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

newtype UpdateRes = UpdateRes
  { _user :: Person
  }
  deriving (Show, Generic, FromJSON, ToSchema)

instance ToJSON UpdateRes where
  toJSON = genericToJSON stripLensPrefixOptions

newtype ListRes = ListRes
  { _users :: [Person]
  }
  deriving (Show, Generic, FromJSON, ToSchema)

instance ToJSON ListRes where
  toJSON = genericToJSON stripLensPrefixOptions

type GetRes = UserInfo
