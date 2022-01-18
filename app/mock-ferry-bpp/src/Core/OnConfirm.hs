module Core.OnConfirm where

import Beckn.Prelude
import Core.Billing
import Core.Fulfillment
import Core.Item
import Core.Payment
import Core.Provider
import Core.Quotation

newtype OnConfirmMessage = OnConfirmMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data State
  = Active
  | CANCELLED
  deriving (Generic, ToJSON, FromJSON, Show)

data Order = Order
  { id :: Text,
    state :: State,
    provider :: ProviderId,
    items :: [OnConfirmItem],
    billing :: Billing,
    fulfillment :: OnConfirmFulfillment,
    quote :: OnConfirmQuotation,
    payment :: OnConfirmPayment
  }
  deriving (Generic, Show, ToJSON, FromJSON)
