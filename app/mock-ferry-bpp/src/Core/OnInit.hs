module Core.OnInit where

import Beckn.Prelude
import Core.Billing
import Core.Fulfillment
import Core.Item
import Core.Payment
import Core.Provider
import Core.Quotation

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
