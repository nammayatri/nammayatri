module Beckn.Types.Mobility.Payload where

import Beckn.Types.Core.Dimensions
import Beckn.Types.Core.Scalar
import Beckn.Types.Mobility.TravelGroup
import Beckn.Types.Mobility.Traveller
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Luggage = Luggage
  { count :: Integer,
    weight :: Scalar,
    dimensions :: Dimensions
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data Payload = Payload
  { luggage :: Maybe Luggage,
    traveller_count :: Maybe Integer,
    travellers :: [Traveller],
    travel_group :: Maybe TravelGroup
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Payload where
  example =
    Payload
      { luggage = Nothing,
        traveller_count = Just 1,
        travellers = example,
        travel_group = Nothing
      }
