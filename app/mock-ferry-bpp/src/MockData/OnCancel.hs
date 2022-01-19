module MockData.OnCancel where

import Beckn.Prelude
import Core.OnCancel
import qualified Core.OnConfirm as OnConfirm
import Core.Payment

failedTransaction :: TrStatus -> OnConfirm.Order -> Order
failedTransaction trStatus = changePaymentState NOT_PAID trStatus . coerceOrder

linkExpired :: OnConfirm.Order -> Order
linkExpired = failedTransaction PaymentLinkExpired

paymentFailed :: OnConfirm.Order -> Order
paymentFailed = failedTransaction Failed
