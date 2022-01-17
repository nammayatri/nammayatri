module Core.Init.Order where

import Beckn.Prelude
import Core.Billing
import Core.Init.Fulfillment
import Core.Init.Item
import Core.Provider

newtype InitMessage = InitMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Order = Order
  { provider :: ProviderId,
    items :: [Item],
    billing :: Billing,
    fulfillment :: FulfillmentId
  }
  deriving (Generic, Show, ToJSON, FromJSON)
