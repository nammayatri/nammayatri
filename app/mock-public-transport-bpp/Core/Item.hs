module Core.Item where

import Beckn.Prelude

newtype Quantity = Quantity
  { count :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)
