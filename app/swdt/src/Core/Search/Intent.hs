module Core.Search.Intent where

import Beckn.Prelude
import Core.Search.Fulfillment

newtype Intent = Intent
  { fulfillment :: Fulfillment
  }
  deriving (Generic, FromJSON, ToJSON, Show)
