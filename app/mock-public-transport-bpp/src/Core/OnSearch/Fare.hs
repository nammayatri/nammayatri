module Core.OnSearch.Fare where

import Beckn.Prelude
import Core.Price

data Fare = Fare
  { id :: Text,
    route_id :: Text,
    price :: Price
  }
  deriving (Show, Generic, FromJSON, ToJSON)
