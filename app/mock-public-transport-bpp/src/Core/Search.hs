module Core.Search where

import Beckn.Prelude
import Beckn.Utils.GenericPretty (PrettyShow)
import Core.Search.Fulfillment

newtype SearchMessage = SearchMessage
  { intent :: Intent
  }
  deriving (Generic, FromJSON, ToJSON, Show, PrettyShow)

newtype Intent = Intent
  { fulfillment :: Fulfillment
  }
  deriving (Generic, FromJSON, ToJSON, Show, PrettyShow)
