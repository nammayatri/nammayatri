module PayoutRejectionTests (payoutRejectionTests) where

import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InvalidRequest), PaymentOrderError (PayoutOrderAlreadyExists), PayoutNotSentError (..))
import Lib.Payment.Payout.Request (isConfirmedRejection)
import Test.Tasty
import Test.Tasty.HUnit

payoutRejectionTests :: TestTree
payoutRejectionTests =
  testGroup
    "Payout create-call failure classification"
    [ testCase "a payout the interface reports as not sent is a confirmed rejection" $
        isConfirmedRejection (toException (PayoutNotSent "no access to connected account")) @?= True,
      testCase "an order id that is already taken is a confirmed rejection" $
        isConfirmedRejection (toException (PayoutOrderAlreadyExists "order-1")) @?= True,
      testCase "a Stripe api_error is not a confirmed rejection" $
        isConfirmedRejection (toException (Payment.ApiError (Payment.StripeErrorInfo Nothing (Just "server error")))) @?= False,
      testCase "an unrelated error is not a confirmed rejection" $
        isConfirmedRejection (toException (InvalidRequest "something else")) @?= False
    ]
