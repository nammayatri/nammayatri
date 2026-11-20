{-# LANGUAGE OverloadedStrings #-}

-- | Collect-payment preview of a manual toll declaration
-- (SharedLogic.ManualTollCharge.previewManualTollChargeResolution).
--
-- The preview must bill exactly what end ride would bill if the driver ended the ride now declaring
-- the amount of the stored request, and must never throw: every case where end ride throws
-- (no request / pending / rejected) reads as "automatic toll, confirmation pending".
module CollectPaymentTollTests (tests) where

import Data.Time (addUTCTime)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import SharedLogic.ManualTollCharge
import Test.Tasty
import Test.Tasty.HUnit

now :: UTCTime
now = read "2026-11-20 10:00:00 UTC"

maxAttempts :: Int
maxAttempts = 3

-- | The configured maxAmountWithoutEstimate.
maxAmount :: HighPrecMoney
maxAmount = 200

-- | A request for 75 made a minute ago with a 120s window (so not yet expired).
record :: ManualTollChargeApprovalStatus -> TollChargeState
record status =
  TollChargeState
    { status,
      amount = 75,
      tollNames = Just ["NICE Road"],
      requestedAt = addUTCTime (-60) now,
      timeoutSeconds = 120,
      rejectionCount = 0,
      rejectedAmounts = [],
      requestId = "req-1"
    }

expired :: TollChargeState -> TollChargeState
expired r = r {requestedAt = addUTCTime (-600) now}

resolution :: HighPrecMoney -> ManualTollChargeConfirmedBy -> [HighPrecMoney] -> Maybe ManualTollChargeResolution
resolution amount confirmedBy rejectedAmounts = Just ManualTollChargeResolution {amount, confirmedBy, rejectedAmounts}

preview :: Maybe TollChargeState -> (Maybe ManualTollChargeResolution, Bool)
preview = previewManualTollChargeResolution maxAmount maxAttempts now

tests :: TestTree
tests =
  testGroup
    "CollectPayment manual toll preview"
    [ testCase "no request yet -> automatic toll, confirmation pending" $
        preview Nothing @?= (Nothing, True),
      testCase "approved by rider -> requested amount, nothing pending" $
        preview (Just (record TOLL_CHARGE_APPROVED_BY_CUSTOMER)) @?= (resolution 75 ConfirmedByCustomer [], False),
      testCase "delivered and window expired -> auto approved, requested amount" $
        preview (Just (expired (record TOLL_CHARGE_DELIVERED_TO_CUSTOMER))) @?= (resolution 75 ConfirmedByTimeout [], False),
      testCase "never delivered and window expired -> no toll" $
        preview (Just (expired (record TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL))) @?= (resolution 0 ConfirmedByNotDelivered [], False),
      testCase "pending within window -> automatic toll, confirmation pending" $
        preview (Just (record TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL)) @?= (Nothing, True),
      testCase "delivered within window -> automatic toll, confirmation pending" $
        preview (Just (record TOLL_CHARGE_DELIVERED_TO_CUSTOMER)) @?= (Nothing, True),
      testCase "rejected below the attempt cap -> automatic toll, confirmation pending" $
        preview (Just (record TOLL_CHARGE_REJECTED_BY_CUSTOMER) {rejectionCount = 1, rejectedAmounts = [75]}) @?= (Nothing, True),
      testCase "rejected at the attempt cap -> unsettled, no toll" $
        preview (Just (record TOLL_CHARGE_REJECTED_BY_CUSTOMER) {rejectionCount = maxAttempts, rejectedAmounts = [75, 70, 65]})
          @?= (resolution 0 ConfirmedByUnsettled [75, 70, 65], False),
      testCase "approved amount above a limit lowered since the request -> automatic toll, confirmation pending" $
        previewManualTollChargeResolution 50 maxAttempts now (Just (record TOLL_CHARGE_APPROVED_BY_CUSTOMER)) @?= (Nothing, True),
      testCase "approved amount equal to the limit -> requested amount" $
        previewManualTollChargeResolution 75 maxAttempts now (Just (record TOLL_CHARGE_APPROVED_BY_CUSTOMER)) @?= (resolution 75 ConfirmedByCustomer [], False),
      testCase "above the limit is checked before the attempt cap, like end ride" $
        previewManualTollChargeResolution 50 maxAttempts now (Just (record TOLL_CHARGE_REJECTED_BY_CUSTOMER) {rejectionCount = maxAttempts, rejectedAmounts = [75, 70, 65]})
          @?= (Nothing, True)
    ]
