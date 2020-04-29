module Beckn.Types.API.LocationBlacklist where

import           Beckn.Types.App
import           Beckn.Types.Common
import           Beckn.Types.Storage.LocationBlacklist
import           Beckn.Utils.Common
import           Data.Default
import           Data.Time.LocalTime
import           EulerHS.Prelude

data CreateReq =
  CreateReq
  { _remarks              :: Text
  , _TenantOrganizationId :: Maybe TenantOrganizationId
  , _type                 :: LocationType
  , _ward                 :: Maybe Text
  , _district             :: Maybe Text
  , _city                 :: Maybe Text
  , _state                :: Maybe Text
  , _country              :: Text
  , _pincode              :: Maybe Int
  , _bound                :: Maybe Bound
  , _info                 :: Maybe Text
  }
  deriving (Show, Generic, ToJSON)

instance FromJSON CreateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions


data CreateRes =
  CreateRes
  { _location_blacklist :: LocationBlacklist
  }
  deriving (Show, Generic, FromJSON, Default)

instance ToJSON CreateRes where
  toJSON = genericToJSON stripLensPrefixOptions

data UpdateReq =
  UpdateReq
  { _remarks              :: Maybe Text
  , _TenantOrganizationId :: Maybe TenantOrganizationId
  , _type                 :: Maybe LocationType
  , _ward                 :: Maybe Text
  , _district             :: Maybe Text
  , _city                 :: Maybe Text
  , _state                :: Maybe Text
  , _country              :: Maybe Text
  , _pincode              :: Maybe Int
  , _bound                :: Maybe Bound
  , _info                 :: Maybe Text
  }
  deriving (Show, Generic, ToJSON)

instance FromJSON UpdateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data UpdateRes =
  UpdateRes
  { _location_blacklist :: LocationBlacklist
  }
  deriving (Show, Generic, FromJSON, Default)

instance ToJSON UpdateRes where
  toJSON = genericToJSON stripLensPrefixOptions

-- List by ward district city state pincode
data ListRes =
  ListRes
  { _location_blacklists :: [LocationBlacklist]
  }
  deriving (Show, Generic, FromJSON, Default)

instance ToJSON ListRes where
  toJSON = genericToJSON stripLensPrefixOptions

type GetRes = LocationBlacklist
