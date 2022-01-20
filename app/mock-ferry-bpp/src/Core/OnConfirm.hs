module Core.OnConfirm where

import Beckn.Prelude
import Core.Billing
import Core.Descriptor
import Core.Fulfillment
import Core.Item
import Core.OrderState
import Core.Payment
import Core.Provider
import Core.Quotation

newtype OnConfirmMessage = OnConfirmMessage
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

data Params = Params
  { transaction_id :: Text,
    transaction_status :: TrStatus,
    amount :: DecimalValue,
    currency :: Text
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data Item = Item
  { id :: Text,
    fulfillment_id :: Text,
    descriptor :: DescriptorCode,
    quantity :: Quantity
  }
  deriving (Generic, Show, ToJSON, FromJSON)

changePaymentState :: Status -> TrStatus -> Order -> Order
changePaymentState st trStatus ord =
  ord{payment =
        ord.payment
          { status = st,
            params = (ord.payment.params :: Params) {transaction_status = trStatus}
          }
     }

successfulPayment :: Order -> Order
successfulPayment = changePaymentState PAID Captured

failedTransaction :: TrStatus -> Order -> Order
failedTransaction = changePaymentState NOT_PAID

linkExpired :: Order -> Order
linkExpired = failedTransaction PaymentLinkExpired

paymentFailed :: Order -> Order
paymentFailed = failedTransaction Failed
