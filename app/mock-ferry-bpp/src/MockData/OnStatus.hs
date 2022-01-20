module MockData.OnStatus where

import Beckn.Prelude
import qualified Core.OnConfirm as OnConfirm
import Core.OnStatus hiding (Order)
import qualified Core.OnStatus as OnStatus
import Core.Payment

successfulPayment :: OnConfirm.Order -> OnStatus.Order
successfulPayment ord = changePaymentState PAID Captured $ coerceOrder ord
