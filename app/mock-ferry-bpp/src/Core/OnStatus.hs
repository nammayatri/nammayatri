module Core.OnStatus where

import Beckn.Prelude
import Core.Billing
import Core.Fulfillment
import Core.Item
import qualified Core.OnConfirm as OnConfirm
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
    items :: [OnStatusItem],
    billing :: Billing,
    fulfillment :: OnStatusFulfillment,
    quote :: OnStatusQuotation,
    payment :: OnStatusPayment
  }
  deriving (Generic, Show, ToJSON, FromJSON)

coerceOrder :: OnConfirm.Order -> Order
coerceOrder order = do
  let id = order.id
      state = order.state
      provider = order.provider
      billing = order.billing
      fulfillment = order.fulfillment
      quote = order.quote
      payment = order.payment
      items = map coerceItem order.items
  Order {..}

changePaymentState :: Status -> TrStatus -> Order -> Order
changePaymentState st trStatus ord =
  ord{payment =
        ord.payment
          { status = st,
            params = ord.payment.params {transaction_status = trStatus}
          }
     }
