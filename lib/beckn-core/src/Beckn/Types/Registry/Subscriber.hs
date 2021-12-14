module Beckn.Types.Registry.Subscriber where

import Data.Aeson (object, withObject, (.:), (.:?), (.=))
import Data.Time (UTCTime)
import EulerHS.Prelude hiding ((.=))
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
  parseJSON = withObject "Subscriber" $ \o ->
    Subscriber
      <$> o .: "ukId"
      <*> o .: "subscriber_id"
      <*> o .:? "subscriber_url"
      <*> o .:? "type"
      <*> o .:? "domain"
      <*> o .:? "city"
      <*> o .:? "country"
      <*> o .: "signing_public_key"
      <*> o .:? "encr_public_key"
      <*> o .:? "valid_from"
      <*> o .:? "valid_until"
      <*> o .:? "status"
      <*> o .:? "created"
      <*> o .:? "updated"

instance ToJSON Subscriber where
  toJSON subscriber =
    object
      [ "ukId" .= subscriber.unique_key_id,
        "subscriber_id" .= subscriber.subscriber_id,
        "subscriber_url" .= subscriber.subscriber_url,
        "type" .= subscriber._type,
        "domain" .= subscriber.domain,
        "city" .= subscriber.city,
        "country" .= subscriber.country,
        "signing_public_key" .= subscriber.signing_public_key,
        "encr_public_key" .= subscriber.encr_public_key,
        "valid_from" .= subscriber.valid_from,
        "valid_until" .= subscriber.valid_until,
        "status" .= subscriber.status,
        "created" .= subscriber.created,
        "updated" .= subscriber.updated
      ]

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
