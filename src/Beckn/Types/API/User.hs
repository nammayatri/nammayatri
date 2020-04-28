module Beckn.Types.API.User where

import           Beckn.Types.App
import           Beckn.Types.Storage.User
import           Data.Default
import           Data.Time.LocalTime
import           EulerHS.Prelude

data CreateReq =
  CreateReq
  { _name           :: Text
  , _username       :: Text
  , _email          :: Text
  , _role           :: Role
  , _organizationId :: OrganizationId
  }
  deriving (Show, Generic, ToJSON)


instance FromJSON CreateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions


data CreateRes =
  CreateRes
  { _user :: User
  }
  deriving (Show, Generic, FromJSON, Default)

instance ToJSON CreateRes where
  toJSON = genericToJSON stripLensPrefixOptions

data UpdateReq =
  UpdateReq
  { _name   :: Maybe Text
  , _email  :: Maybe Text
  , _role   :: Maybe Role
  , _status :: Status
  }
  deriving (Show, Generic, ToJSON)

instance FromJSON UpdateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data UpdateRes =
  UpdateRes
  { _user :: User
  }
  deriving (Show, Generic, FromJSON, Default)

instance ToJSON UpdateRes where
  toJSON = genericToJSON stripLensPrefixOptions


data ListRes =
  ListRes
  { _users :: [User]
  }
  deriving (Show, Generic, FromJSON, Default)

instance ToJSON ListRes where
  toJSON = genericToJSON stripLensPrefixOptions

type GetRes = User
