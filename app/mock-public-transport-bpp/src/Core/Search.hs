module Core.Search where

import Beckn.Prelude
import Core.Search.Fulfillment

newtype SearchMessage = SearchMessage
  { intent :: Intent
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype Intent = Intent
  { fulfillment :: Fulfillment
  }
  deriving (Generic, FromJSON, ToJSON, Show)


