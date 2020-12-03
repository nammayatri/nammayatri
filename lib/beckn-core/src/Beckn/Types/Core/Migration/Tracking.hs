module Beckn.Types.Core.Migration.Tracking where

import Beckn.Utils.JSON (constructorsToLowerOptions)
import Data.Aeson.Types (Value (..), typeMismatch)
import EulerHS.Prelude
import Servant.Client (BaseUrl)

data Tracking = Tracking
  { _tl_method :: Maybe TlMethod,
    _url :: Maybe BaseUrl,
    _status :: Maybe TrackingStatus
  }
  deriving (Generic, Show)

data TlMethod = HttpGet | WS
  deriving (Show)

instance FromJSON TlMethod where
  parseJSON (String "http/get") = pure HttpGet
  parseJSON (String "ws") = pure WS
  parseJSON e = typeMismatch "tl_method" e

instance ToJSON TlMethod where
  toJSON HttpGet = String "http/get"
  toJSON WS = String "ws"

data TrackingStatus = ACTIVE | INACTIVE
  deriving (Generic, Show)

instance FromJSON TrackingStatus where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON TrackingStatus where
  toJSON = genericToJSON constructorsToLowerOptions

instance FromJSON Tracking where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Tracking where
  toJSON = genericToJSON stripLensPrefixOptions
