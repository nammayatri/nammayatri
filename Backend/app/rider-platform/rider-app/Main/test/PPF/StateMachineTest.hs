module PPF.StateMachineTest (stateMachineTests) where

import qualified Data.List as L
import Domain.Types.PPFRecon
import Kernel.Prelude
import SharedLogic.PPF.PaymentStateMachine
import Test.Tasty
import Test.Tasty.HUnit

stateMachineTests :: TestTree
stateMachineTests =
  testGroup
    "PPF State Machine"
    [ paymentTransitionTests,
      settlementTransitionTests
    ]

-- | All PPFPaymentStatus constructors for exhaustive testing
allPaymentStatuses :: [PPFPaymentStatus]
allPaymentStatuses = [INITIATED, COLLECTED, HELD, RELEASED, SETTLED, REFUNDED, FAILED]

-- | All PPFSettlementStatus constructors for exhaustive testing
allSettlementStatuses :: [PPFSettlementStatus]
allSettlementStatuses = [PENDING, IN_PROGRESS, SETTLEMENT_SETTLED, SETTLEMENT_FAILED]

paymentTransitionTests :: TestTree
paymentTransitionTests =
  testGroup
    "Payment Status Transitions"
    [ testCase "Valid: INITIATED -> COLLECTED" $
        assertValidPayment INITIATED COLLECTED,
      testCase "Valid: COLLECTED -> HELD" $
        assertValidPayment COLLECTED HELD,
      testCase "Valid: HELD -> RELEASED" $
        assertValidPayment HELD RELEASED,
      testCase "Valid: RELEASED -> SETTLED" $
        assertValidPayment RELEASED SETTLED,
      testCase "Valid: HELD -> REFUNDED" $
        assertValidPayment HELD REFUNDED,
      testCase "Valid: COLLECTED -> REFUNDED" $
        assertValidPayment COLLECTED REFUNDED,
      testCase "Valid: INITIATED -> FAILED" $
        assertValidPayment INITIATED FAILED,
      testCase "Valid: COLLECTED -> FAILED" $
        assertValidPayment COLLECTED FAILED,
      testCase "Invalid: SETTLED -> INITIATED (no backward transition)" $
        assertInvalidPayment SETTLED INITIATED,
      testCase "Invalid: FAILED -> COLLECTED (terminal state)" $
        assertInvalidPayment FAILED COLLECTED,
      testCase "Invalid: REFUNDED -> SETTLED (terminal state)" $
        assertInvalidPayment REFUNDED SETTLED,
      testCase "No self-transitions in payment statuses" $
        forM_ allPaymentStatuses $ \s ->
          assertBool
            ("Self-transition not allowed: " <> show s <> " -> " <> show s)
            ((s, s) `notElem` validPaymentTransitions),
      testCase "Every payment status appears at least once" $ do
        let sources = map fst validPaymentTransitions
            targets = map snd validPaymentTransitions
            allMentioned = L.nub (sources <> targets)
        forM_ allPaymentStatuses $ \s ->
          assertBool
            ("Payment status " <> show s <> " appears in transitions")
            (s `elem` allMentioned)
    ]

settlementTransitionTests :: TestTree
settlementTransitionTests =
  testGroup
    "Settlement Status Transitions"
    [ testCase "Valid: PENDING -> IN_PROGRESS" $
        assertValidSettlement PENDING IN_PROGRESS,
      testCase "Valid: IN_PROGRESS -> SETTLEMENT_SETTLED" $
        assertValidSettlement IN_PROGRESS SETTLEMENT_SETTLED,
      testCase "Valid: IN_PROGRESS -> SETTLEMENT_FAILED" $
        assertValidSettlement IN_PROGRESS SETTLEMENT_FAILED,
      testCase "Valid: PENDING -> SETTLEMENT_FAILED" $
        assertValidSettlement PENDING SETTLEMENT_FAILED,
      testCase "Valid: SETTLEMENT_FAILED -> PENDING (retry)" $
        assertValidSettlement SETTLEMENT_FAILED PENDING,
      testCase "Invalid: SETTLEMENT_SETTLED -> PENDING (no retry after success)" $
        assertInvalidSettlement SETTLEMENT_SETTLED PENDING,
      testCase "Invalid: SETTLEMENT_SETTLED -> SETTLEMENT_FAILED" $
        assertInvalidSettlement SETTLEMENT_SETTLED SETTLEMENT_FAILED,
      testCase "No self-transitions in settlement statuses" $
        forM_ allSettlementStatuses $ \s ->
          assertBool
            ("Self-transition not allowed: " <> show s <> " -> " <> show s)
            ((s, s) `notElem` validSettlementTransitions),
      testCase "Every settlement status appears at least once" $ do
        let sources = map fst validSettlementTransitions
            targets = map snd validSettlementTransitions
            allMentioned = L.nub (sources <> targets)
        forM_ allSettlementStatuses $ \s ->
          assertBool
            ("Settlement status " <> show s <> " appears in transitions")
            (s `elem` allMentioned)
    ]

-- | Assert that a payment transition is valid
assertValidPayment :: PPFPaymentStatus -> PPFPaymentStatus -> Assertion
assertValidPayment from to =
  assertBool
    ("Expected valid transition: " <> show from <> " -> " <> show to)
    ((from, to) `elem` validPaymentTransitions)

-- | Assert that a payment transition is invalid
assertInvalidPayment :: PPFPaymentStatus -> PPFPaymentStatus -> Assertion
assertInvalidPayment from to =
  assertBool
    ("Expected invalid transition: " <> show from <> " -> " <> show to)
    ((from, to) `notElem` validPaymentTransitions)

-- | Assert that a settlement transition is valid
assertValidSettlement :: PPFSettlementStatus -> PPFSettlementStatus -> Assertion
assertValidSettlement from to =
  assertBool
    ("Expected valid transition: " <> show from <> " -> " <> show to)
    ((from, to) `elem` validSettlementTransitions)

-- | Assert that a settlement transition is invalid
assertInvalidSettlement :: PPFSettlementStatus -> PPFSettlementStatus -> Assertion
assertInvalidSettlement from to =
  assertBool
    ("Expected invalid transition: " <> show from <> " -> " <> show to)
    ((from, to) `notElem` validSettlementTransitions)
