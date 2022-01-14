module Beckn.Types.Registry.API where

import Beckn.Types.Registry.City (City)
import Beckn.Types.Registry.Country (Country)
import Beckn.Types.Registry.Domain (Domain)
import Beckn.Types.Registry.Subscriber (Subscriber, SubscriberType)
import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data LookupRequest = LookupRequest
  { unique_key_id :: Maybe Text,
    subscriber_id :: Maybe Text,
    _type :: Maybe SubscriberType,
    domain :: Maybe Domain,
    country :: Maybe Country,
    city :: Maybe City
  }
  deriving (Show, Generic, ToSchema)

emptyLookupRequest :: LookupRequest
emptyLookupRequest = LookupRequest Nothing Nothing Nothing Nothing Nothing Nothing

instance FromJSON LookupRequest where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LookupRequest where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

type LookupResponse = [Subscriber]
