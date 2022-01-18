module Core.Search where

import Beckn.Prelude
import Core.Fulfillment

newtype SearchMessage = SearchMessage
  { intent :: Intent
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype Intent = Intent
  { fulfillment :: SearchFulfillment
  }
  deriving (Generic, FromJSON, ToJSON, Show)
