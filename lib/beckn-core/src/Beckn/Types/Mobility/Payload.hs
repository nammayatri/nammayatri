module Beckn.Types.Mobility.Payload where

import Beckn.Types.Core.Dimensions
import Beckn.Types.Core.Scalar
import Beckn.Types.Mobility.TravelGroup
import Beckn.Types.Mobility.Traveller
import Beckn.Utils.Example
import Beckn.Utils.JSON
import EulerHS.Prelude

data Luggage = Luggage
  { count :: Integer,
    weight :: Scalar,
    dimensions :: Dimensions
  }
  deriving (Generic, Show)

instance FromJSON Luggage where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Luggage where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Payload = Payload
  { luggage :: Maybe Luggage,
    traveller_count :: Maybe Integer,
    travellers :: [Traveller],
    travel_group :: Maybe TravelGroup
  }
  deriving (Generic, Show)

instance FromJSON Payload where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payload where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Payload where
  example =
    Payload
      { luggage = Nothing,
        traveller_count = Just 1,
        travellers = example,
        travel_group = Nothing
      }
