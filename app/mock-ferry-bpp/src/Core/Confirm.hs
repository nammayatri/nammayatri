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
    items :: [Item],
    billing :: Billing,
    fulfillment :: FulfillmentId,
    quote :: Quotation,
    payment :: Payment Params
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Item = Item
  { id :: Text,
    fulfillment_id :: Text,
    quantity :: Quantity
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Params = Params
  { amount :: DecimalValue,
    currency :: Text
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON)
