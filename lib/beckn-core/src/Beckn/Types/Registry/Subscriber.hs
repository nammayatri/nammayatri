module Beckn.Types.Registry.Subscriber where

import Beckn.Types.Base64
import Data.Aeson
import Data.Time (UTCTime)
import EulerHS.Prelude hiding ((.=))
import Servant.Client (BaseUrl)

data Subscriber = Subscriber
  { unique_key_id :: Text,
    subscriber_id :: Text,
    subscriber_url :: BaseUrl,
    _type :: Maybe SubscriberType,
    domain :: Maybe Text,
    city :: Maybe Text,
    country :: Maybe Text,
    signing_public_key :: Base64,
    encr_public_key :: Maybe Base64,
    valid_from :: Maybe UTCTime,
    valid_until :: Maybe UTCTime,
    status :: Maybe SubscriberStatus,
    created :: Maybe UTCTime,
    updated :: Maybe UTCTime
  }
  deriving (Show, Generic)

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "unique_key_id" -> "ukId"
        "_type" -> "type"
        other -> other
    }

instance FromJSON Subscriber where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Subscriber where
  toJSON = genericToJSON jsonOptions

data SubscriberType
  = BAP
  | BPP
  | BG
  | LREG
  | CREG
  | RREG
  deriving (Show, Generic, FromJSON, ToJSON)

data SubscriberStatus
  = INITIATED
  | UNDER_SUBSCRIPTION
  | SUBSCRIBED
  | EXPIRED
  | UNSUBSCRIBED
  | INVALID_SSL
  deriving (Show, Generic, FromJSON, ToJSON)
