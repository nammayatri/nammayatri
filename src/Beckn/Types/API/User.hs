module Beckn.Types.API.User where

import           Beckn.Types.Storage.User
import           Data.Time.LocalTime
import           EulerHS.Prelude
----------
-- Create
----------
data CreateReq =
  CreateReq
  { _name     :: Text
  , _username :: Text
  , _email    :: Text
  , _role     :: Text
  }
  deriving (Show, Generic, ToJSON)


instance FromJSON CreateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions


data CreateRes =
  CreateRes
  { _user :: User
  }
  deriving (Show, Generic, FromJSON, ToJSON)

----------
-- Update
----------
data UpdateReq =
  UpdateReq
  { _name   :: Maybe Text
  , _email  :: Text
  , _role   :: Text
  , _status :: Text
  }
  deriving (Show, Generic, ToJSON)

instance FromJSON UpdateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data UpdateRes =
  UpdateRes
  { _user :: User
  }
  deriving (Show, Generic, FromJSON, ToJSON)

----------
-- List
----------
data ListRes =
  ListRes
  { _users :: [User]
  }
  deriving (Show, Generic, ToJSON, FromJSON)
