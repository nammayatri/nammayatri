module Core.OnConfirm where

import Core.Common.Payment
import Core.OnConfirm.Order
import Core.OnConfirm.Params
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
successfulPayment = changePaymentState PAID CAPTURED

failedTransaction :: TrStatus -> Order -> Order
failedTransaction = changePaymentState NOT_PAID

linkExpired :: Order -> Order
linkExpired = failedTransaction PAYMENT_LINK_EXPIRED

paymentFailed :: Order -> Order
paymentFailed = failedTransaction FAILED
