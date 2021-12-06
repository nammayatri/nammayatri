module Core.Confirm.Order where

import Beckn.Prelude
import Core.Confirm.Billing
import Core.Confirm.Fulfillment
import Core.Confirm.Item
import Core.Confirm.Provider

data Order = Order
  { provider :: Provider,
    items :: [Item],
    billing :: Billing,
    fulfillment :: Fulfillment
  }
  deriving (Generic, ToJSON)
