module Core1.OnInit where

import Beckn.Prelude
import Core1.Billing
import Core1.Fulfillment
import Core1.Item
import Core1.Payment
import Core1.Provider
import Core1.Quotation

newtype OnInitMessage = OnInitMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Order = Order
  { provider :: ProviderId,
    items :: [InitItem],
    billing :: Billing,
    fulfillment :: OnInitFulfillment,
    quote :: OnInitQuotation,
    payment :: OnInitPayment
  }
  deriving (Generic, Show, ToJSON, FromJSON)
