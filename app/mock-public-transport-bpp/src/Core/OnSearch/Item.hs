module Core.OnSearch.Item where

import Beckn.Prelude

data Item = Item
  { departure_id :: Text,
    fare_id :: Text,
    canBook :: Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON)
