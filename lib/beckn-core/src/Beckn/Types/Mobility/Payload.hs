module Beckn.Types.Mobility.Payload where

import Beckn.Types.Core.Dimensions
import Beckn.Types.Core.Scalar
import Beckn.Types.Mobility.TravelGroup
import Beckn.Types.Mobility.Traveller
import Beckn.Utils.Example
import EulerHS.Prelude

data Luggage = Luggage
  { _count :: Integer,
    _weight :: Scalar,
    _dimensions :: Dimensions
  }
  deriving (Generic, Show)

instance FromJSON Luggage where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Luggage where
  toJSON = genericToJSON stripLensPrefixOptions

data Payload = Payload
  { _luggage :: Maybe Luggage,
    _traveller_count :: Maybe Integer,
    _travellers :: [Traveller],
    _travel_group :: Maybe TravelGroup
  }
  deriving (Generic, Show)

instance FromJSON Payload where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Payload where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example Payload where
  example =
    Payload
      { _luggage = Nothing,
        _traveller_count = Just 1,
        _travellers = example,
        _travel_group = Nothing
      }
