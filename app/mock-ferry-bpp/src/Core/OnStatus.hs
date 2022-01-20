module Core.OnStatus where

import Beckn.Prelude
import Core.Billing
import Core.Fulfillment
import Core.Item
import Core.OrderState
import Core.Payment
import Core.Provider
import Core.Quotation

newtype OnStatusMessage = OnStatusMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Order = Order
  { id :: Text,
    state :: State,
    provider :: ProviderId,
    items :: [Item],
    billing :: Billing,
    fulfillment :: FullInfoFulfillment,
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
  { transaction_id :: Text,
    transaction_status :: TrStatus,
    amount :: DecimalValue,
    currency :: Text
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON)
