module Core1.OnConfirm where

import Beckn.Prelude
import Core1.Billing
import Core1.Fulfillment
import Core1.Item
import Core1.Payment
import Core1.Provider
import Core1.Quotation

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
