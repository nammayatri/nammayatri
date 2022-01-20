module Core.Confirm where

import Beckn.Prelude
import Core.Billing
import Core.Fulfillment
import Core.Item
import Core.Payment
import Core.Provider
import Core.Quotation

newtype ConfirmMessage = ConfirmMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Order = Order
  { provider :: ProviderId,
    items :: [ConfirmItem],
    billing :: Billing,
    fulfillment :: FulfillmentId,
    quote :: ConfirmQuotation,
    payment :: ConfirmPayment
  }
  deriving (Generic, Show, ToJSON, FromJSON)
