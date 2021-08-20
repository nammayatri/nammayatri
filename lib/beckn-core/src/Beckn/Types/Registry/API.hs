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

lookupAPI ::
  Proxy
    ( "lookup"
        :> ReqBody '[JSON] LookupRequest
        :> Post '[JSON] LookupResponse
    )
lookupAPI = Proxy

data LookupRequest = LookupRequest
  { subscriber_id :: Maybe Text,
    _type :: Maybe ParticipantRole,
    domain :: Maybe Domain,
    country :: Maybe Country,
    city :: Maybe City
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
