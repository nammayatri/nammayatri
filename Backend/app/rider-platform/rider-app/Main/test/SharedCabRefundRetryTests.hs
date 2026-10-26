{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | The refund-retry attempt cap, pure-tested by simulating failure streams against the sweep's decision fn
-- (`SharedLogic.SharedCab.RefundRetry.retryStep`): no Redis, no payment network.
module SharedCabRefundRetryTests (tests) where

import qualified "rider-app" Domain.Types.FRFSTicketBooking as DFTB
import qualified "rider-app" Domain.Types.MerchantOperatingCity as DMOC
import "mobility-core" Kernel.Types.Id (Id (..))
import "rider-app" SharedLogic.SharedCab.Keys (refundRetryCityKey, refundRetryKey)
import "rider-app" SharedLogic.SharedCab.RefundRetry (PaymentState (..), RetryAction (..), RetryStep (..), decideRetryStep, maxRefundRetryAttempts, refundRetryTtlSec, retryStep)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Prelude

data SimEvent
  = -- | the sweep attempted startRefund, at attempt number n
    Tried Int
  | -- | the attempt started the refund: the marker is cleared, the booking leaves the set, no more visits
    Cleared
  | -- | the cap held: ops-alert, marker cleared (the alert fires exactly once -- the set no longer re-drives it)
    Alerted
  deriving (Show, Eq)

-- | @steps cap attempts\` outcomes@ replays the sweep for one booking; each entry is one scheduled refund attempt, False means it failed again.
steps :: Int -> Int -> [Bool] -> [SimEvent]
steps _ _ [] = []
steps cap attempts (ok : rest) = case retryStep cap attempts of
  GiveUp -> [Alerted]
  AttemptRefund n
    | ok -> [Tried n, Cleared]
    | otherwise -> Tried n : steps cap n rest

alwaysFails :: Int -> [Bool]
alwaysFails n = replicate n False

tests :: TestTree
tests =
  testGroup
    "shared-cab refund retry"
    ( [ testCase "retryStep caps: attempts below the max retry, at and above it the sweep gives up" $
          map (retryStep maxRefundRetryAttempts) [0, 1, 4, 5, 100] @?= [AttemptRefund 1, AttemptRefund 2, AttemptRefund 5, GiveUp, GiveUp],
        testCase "five consecutive failures cost five real attempts, then exactly one ops-alert" $
          steps maxRefundRetryAttempts 0 (alwaysFails 60) @?= [Tried 1, Tried 2, Tried 3, Tried 4, Tried 5, Alerted],
        testCase "a success on any attempt ends the series with no alert" $
          map (steps maxRefundRetryAttempts 0) [[True], [False, False, True]]
            @?= [[Tried 1, Cleared], [Tried 1, Tried 2, Tried 3, Cleared]],
        testCase "after cap failures the give-up fires BEFORE the refund is attempted again (a queued success never runs)" $
          steps maxRefundRetryAttempts 0 (replicate maxRefundRetryAttempts False ++ [True]) @?= map Tried [1 .. maxRefundRetryAttempts] ++ [Alerted],
        testCase "a success on the cap attempt is still a success (the cap binds only the NEXT decision)" $
          steps maxRefundRetryAttempts 0 (replicate (maxRefundRetryAttempts - 1) False <> [True]) @?= map Tried [1 .. maxRefundRetryAttempts] <> [Cleared],
        testCase "the alert fires at most once per marker lifetime (GiveUp removes the booking from the visited set)" $
          length (filter (== Alerted) (steps maxRefundRetryAttempts 0 (alwaysFails 60))) @?= 1,
        testCase "markers are per booking + per city and carry a days-scale TTL" $ do
          let b1 = Id "booking-1" :: Id DFTB.FRFSTicketBooking
              b2 = Id "booking-2" :: Id DFTB.FRFSTicketBooking
              c1 = Id "city-1" :: Id DMOC.MerchantOperatingCity
              c2 = Id "city-2" :: Id DMOC.MerchantOperatingCity
          assertBool "booking key must name the booking" (refundRetryKey b1 == "sharedcab:refundretry:booking-1")
          assertBool "per-booking keys differ" (refundRetryKey b1 /= refundRetryKey b2)
          assertBool "per-city keys differ" (refundRetryCityKey c1 /= refundRetryCityKey c2)
          assertBool "TTL is a couple of days" (refundRetryTtlSec >= 2 * 24 * 3600)
      ]
        ++ h2StateMatrix
    )

-- | The refund pass decides idempotently from the payment evidence BEFORE any startRefund (refund records first, then row
-- status): started/settled evidence unmarks, only owed evidence retries, a missing payment row is terminal, and a stale
-- REFUND_PENDING with no record is owed.
h2StateMatrix :: [TestTree]
h2StateMatrix =
  [ testGroup
      "decideRetryStep: idempotency before startRefund (H2, record-over-status per H3)"
      [ testCase "an existing refund record on the order => Done, whatever the status column says" $
          decideRetryStep PaymentRefundRecord @?= Done,
        testCase "REFUND_INITIATED with no record on this read => Done (that status is stamped only from a refunds row in transit)" $
          decideRetryStep PaymentRefundInitiatedNoRecord @?= Done,
        testCase "REFUNDED with no record on this read => Done (settled already)" $
          decideRetryStep PaymentRefundedNoRecord @?= Done,
        testCase "H3: REFUND_PENDING with NO refund record => Start (the pre-gateway stale mark; silent Done would leave the money un-refunded)" $
          decideRetryStep PaymentRefundPendingNoRecord @?= Start,
        testCase "missing payment row => Terminal (aligned with the cancel-start path: no payment to refund; the marker must not loop)" $
          decideRetryStep PaymentMissing @?= Terminal,
        testCase "a payment row with no refund evidence retries the start" $
          decideRetryStep PaymentOwed @?= Start,
        testCase "mutation guards: owed states are not Done/Terminal, settled/stale evidence is not confused, missing is not Start" $ do
          (decideRetryStep PaymentOwed == Done || decideRetryStep PaymentOwed == Terminal) @?= False
          (decideRetryStep PaymentRefundPendingNoRecord == Done || decideRetryStep PaymentRefundPendingNoRecord == Terminal) @?= False
          decideRetryStep PaymentRefundRecord @?= Done
          decideRetryStep PaymentMissing @?= Terminal
      ]
  ]
