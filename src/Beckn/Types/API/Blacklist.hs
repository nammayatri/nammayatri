module Beckn.Types.API.Blacklist where

import           Beckn.Types.App
import           Beckn.Types.Common
import           Beckn.Types.Storage.Blacklist
import           Beckn.Utils.Common
import           Data.Default
import           Data.Time.LocalTime
import           EulerHS.Prelude

data CreateReq =
  CreateReq
  { _remarks              :: Text
  , _TenantOrganizationId :: Maybe TenantOrganizationId
  , _EntityId             :: Text
  , _entityType           :: EntityType
  , _startTime            :: LocalTime
  , _endTime              :: LocalTime
  , _info                 :: Maybe Text

  }
  deriving (Show, Generic, ToJSON)

instance FromJSON CreateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions


data CreateRes =
  CreateRes
  { _blacklist :: Blacklist
  }
  deriving (Show, Generic, FromJSON, Default)

instance ToJSON CreateRes where
  toJSON = genericToJSON stripLensPrefixOptions

data UpdateReq =
  UpdateReq
  { _remarks              :: Maybe Text
  , _TenantOrganizationId :: Maybe TenantOrganizationId
  , _EntityId             :: Maybe Text
  , _entityType           :: Maybe EntityType
  , _info                 :: Maybe Text
  , _startTime            :: Maybe LocalTime
  , _endTime              :: Maybe LocalTime
  }
  deriving (Show, Generic, ToJSON)

instance FromJSON UpdateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data UpdateRes =
  UpdateRes
  { _blacklist :: Blacklist
  }
  deriving (Show, Generic, FromJSON, Default)

instance ToJSON UpdateRes where
  toJSON = genericToJSON stripLensPrefixOptions


data ListRes =
  ListRes
  { _blacklists :: [Blacklist]
  }
  deriving (Show, Generic, FromJSON, Default)

instance ToJSON ListRes where
  toJSON = genericToJSON stripLensPrefixOptions

type GetRes = Blacklist
