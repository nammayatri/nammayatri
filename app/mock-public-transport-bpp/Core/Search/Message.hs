module Core.Search.Message where

import Beckn.Prelude
import Core1.Fulfillment

newtype Message = Message
  { intent :: Intent
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype Intent = Intent
  { fulfillment :: SearchFulfillment
  }
  deriving (Generic, FromJSON, ToJSON, Show)
