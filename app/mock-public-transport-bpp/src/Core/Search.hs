module Core.Search where

import Beckn.Prelude
import Core.Search.Fulfillment
import Beckn.Utils.GenericPretty (PrettyShow)

newtype SearchMessage = SearchMessage
  { intent :: Intent
  }
  deriving (Generic, FromJSON, ToJSON, Show, PrettyShow)

newtype Intent = Intent
  { fulfillment :: Fulfillment
  }
  deriving (Generic, FromJSON, ToJSON, Show, PrettyShow)


