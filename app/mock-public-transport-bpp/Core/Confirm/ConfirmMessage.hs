module Core.Confirm.ConfirmMessage where

import Beckn.Prelude
import Core.Billing
import Core.Init.Fulfillment
import Core.OnInit.Item
import Core.OnInit.Payment
import Core.OnInit.Quotation
import Core.Provider

newtype ConfirmMessage = ConfirmMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Order = Order
  { provider :: ProviderId,
    items :: [Item],
    billing :: Billing,
    fulfillment :: FulfillmentId,
    quote :: Quotation,
    payment :: Payment
  }
  deriving (Generic, Show, ToJSON, FromJSON)
