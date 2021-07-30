module Beckn.Types.Registry.API where

import Beckn.Types.Registry.City (City)
import Beckn.Types.Registry.Country (Country)
import Beckn.Types.Registry.Domain (Domain)
import Beckn.Types.Registry.Subscriber (Subscriber)
import Beckn.Utils.JSON (constructorsToLowerOptions, stripPrefixUnderscoreIfAny)
import Beckn.Utils.Servant.SignatureAuth (SignatureAuth)
import EulerHS.Prelude
import Servant

type LookupAPI registryLookup =
  SignatureAuth "Signature" registryLookup
    :> "lookup"
    :> ReqBody '[JSON] LookupRequest
    :> Post '[JSON] LookupResponse

data LookupRequest = LookupRequest
  { subscriber_id :: Text,
    _type :: ParticipantRole,
    domain :: Domain,
    country :: Country,
    city :: City
  }
  deriving (Show, Generic)

instance FromJSON LookupRequest where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LookupRequest where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data ParticipantRole = BAP | BPP | BG
  deriving (Show, Generic)

instance FromJSON ParticipantRole where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON ParticipantRole where
  toJSON = genericToJSON constructorsToLowerOptions

type LookupResponse = [Subscriber]
