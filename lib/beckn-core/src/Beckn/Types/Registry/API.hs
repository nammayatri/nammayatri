module Beckn.Types.Registry.API where

import Beckn.Types.Registry.City (City)
import Beckn.Types.Registry.Country (Country)
import Beckn.Types.Registry.Domain (Domain)
import Beckn.Types.Registry.Subscriber (Subscriber)
import Beckn.Utils.JSON (constructorsToLowerOptions, stripPrefixUnderscoreIfAny)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data LookupRequest = LookupRequest
  { subscriber_id :: Maybe Text,
    _type :: Maybe ParticipantRole,
    domain :: Maybe Domain,
    country :: Maybe Country,
    city :: Maybe City
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON LookupRequest where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LookupRequest where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data ParticipantRole = BAP | BPP | BG
  deriving (Show, Generic, ToSchema)

instance FromJSON ParticipantRole where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON ParticipantRole where
  toJSON = genericToJSON constructorsToLowerOptions

type LookupResponse = [Subscriber]

newtype OnSubscribeRequest = OnSubscribeRequest {challenge :: Text}
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype OnSubscribeResponse = OnSubscribeResponse {answer :: Text}
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
