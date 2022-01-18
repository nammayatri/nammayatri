module Core.Init where

import Beckn.Prelude
import Core.Billing
import Core.Fulfillment
import Core.Item
import Core.Provider

newtype InitMessage = InitMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Order = Order
  { provider :: ProviderId,
    items :: [InitItem],
    billing :: Billing,
    fulfillment :: FulfillmentId
  }
  deriving (Generic, Show, ToJSON, FromJSON)
