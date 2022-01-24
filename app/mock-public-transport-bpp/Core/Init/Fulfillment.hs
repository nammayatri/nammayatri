module Core.Init.Fulfillment where

import Beckn.Prelude

newtype FulfillmentId = FulfillmentId
  { id :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)
