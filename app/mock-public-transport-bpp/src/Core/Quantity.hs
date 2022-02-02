module Core.Quantity where

import Beckn.Prelude

newtype Quantity = Quantity
  { count :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)
