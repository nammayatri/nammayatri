module Core1.Confirm where

import Beckn.Prelude
import Core1.Billing
import Core1.Fulfillment
import Core1.Item
import Core1.Payment
import Core1.Provider
import Core1.Quotation

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
    payment :: OnInitPayment
  }
  deriving (Generic, Show, ToJSON, FromJSON)
