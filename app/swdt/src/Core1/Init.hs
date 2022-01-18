module Core1.Init where

import Beckn.Prelude
import Core1.Billing
import Core1.Fulfillment
import Core1.Item
import Core1.Provider

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
