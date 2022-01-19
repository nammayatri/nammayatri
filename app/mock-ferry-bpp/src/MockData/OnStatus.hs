module MockData.OnStatus where

import Beckn.Prelude
import qualified Core.OnConfirm as OnConfirm
import Core.OnStatus
import Core.Payment

successfulPayment :: OnConfirm.Order -> Order
successfulPayment ord = changePaymentState PAID Captured $ coerceOrder ord
