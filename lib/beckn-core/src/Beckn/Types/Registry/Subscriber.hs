module Beckn.Types.Registry.Subscriber where

import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import Data.Time (UTCTime)
import EulerHS.Prelude
import Servant.Client (BaseUrl)

data Subscriber = Subscriber
  { unique_key_id :: Text,
    subscriber_id :: Text,
    subscriber_url :: Maybe BaseUrl,
    _type :: Maybe SubscriberType,
    domain :: Maybe Text,
    city :: Maybe Text,
    country :: Maybe Text,
    signing_public_key :: Text,
    encr_public_key :: Maybe Text,
    valid_from :: Maybe UTCTime,
    valid_until :: Maybe UTCTime,
    status :: Maybe SubscriberStatus,
    created :: Maybe UTCTime,
    updated :: Maybe UTCTime
  }
  deriving (Show, Generic)

instance FromJSON Subscriber where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Subscriber where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

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
