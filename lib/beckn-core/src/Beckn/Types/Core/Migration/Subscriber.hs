module Beckn.Types.Core.Migration.Subscriber where

import Beckn.Types.Core.Migration.Domain
import Beckn.Utils.JSON (constructorsToLowerOptions)
import Data.Time
import EulerHS.Prelude

data Subscriber = Subscriber
  { _subscriber_id :: Maybe Text,
    _type :: Maybe SubscriberType,
    _cb_url :: Maybe Text,
    _domain :: Maybe Domain,
    _city :: Maybe Text,
    _country :: Maybe Text,
    _signing_public_key :: Maybe Text,
    _encryption_public_key :: Maybe Text,
    _status :: Maybe SubscriberStatus,
    _created :: Maybe UTCTime,
    _updated :: Maybe UTCTime,
    _expires :: Maybe UTCTime
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
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Subscriber where
  toJSON = genericToJSON stripAllLensPrefixOptions
