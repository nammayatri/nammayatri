module Core.Spec.OnConfirm where

import Beckn.Prelude
import Beckn.Utils.GenericPretty
import Core.Spec.Common.Payment
import Core.Spec.OnConfirm.Order
import Core.Spec.OnConfirm.Params

newtype OnConfirmMessage = OnConfirmMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, PrettyShow)

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
