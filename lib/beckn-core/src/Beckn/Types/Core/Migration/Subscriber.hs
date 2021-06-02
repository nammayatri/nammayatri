module Beckn.Types.Core.Migration.Subscriber where

import Beckn.Types.Core.Migration.Domain
import Beckn.Utils.JSON
import Data.Time
import EulerHS.Prelude

data Subscriber = Subscriber
  { subscriber_id :: Maybe Text,
    _type :: Maybe SubscriberType,
    cb_url :: Maybe Text,
    domain :: Maybe Domain,
    city :: Maybe Text,
    country :: Maybe Text,
    signing_public_key :: Maybe Text,
    encryption_public_key :: Maybe Text,
    status :: Maybe SubscriberStatus,
    created :: Maybe UTCTime,
    updated :: Maybe UTCTime,
    expires :: Maybe UTCTime
  }
  deriving (Generic, Show)

data SubscriberType
  = BAP
  | BPP
  | BG
  | BPPR
  | BGR
  deriving (Generic, Show, Eq)

data SubscriberStatus
  = INITIATED
  | UNDER_SUBSCRIPTION
  | SUBSCRIBED
  | INVALID_SSL
  | UNSUBSCRIBED
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance FromJSON SubscriberType where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON SubscriberType where
  toJSON = genericToJSON constructorsToLowerOptions

instance FromJSON Subscriber where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Subscriber where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
