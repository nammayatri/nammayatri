module Core.OnConfirm where

import Core.OnConfirm.Order
import Core.OnConfirm.Params
import Core.Common.Payment
import Data.Aeson
import Relude hiding (State, id, ord, state)

newtype OnConfirmMessage = OnConfirmMessage
  { order :: Order
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
