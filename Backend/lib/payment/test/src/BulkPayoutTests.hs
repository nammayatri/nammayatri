module BulkPayoutTests (bulkPayoutTests) where

import Data.Either (isLeft)
import qualified Data.Time as Time
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import Lib.Payment.Payout.Bulk.Resolve (allOrdersFinal, unsettledCount)
import Lib.Payment.Payout.Bulk.Schedule
import Lib.Payment.Payout.Bulk.Types (isPayoutRequestFinal)
import Lib.Payment.Payout.HoldRelease
import Test.Tasty
import Test.Tasty.HUnit

bulkPayoutTests :: TestTree
bulkPayoutTests =
  testGroup
    "HDFC bulk payout status checks (Lib.Payment.Payout.Bulk)"
    [ scheduleTests,
      callTests,
      batchCompletionTests,
      settlementTests,
      holdReleaseTests,
      recheckTests,
      closeAfterReleaseTests
    ]

-- A small local-stack plan: checks at 2, 4 and 6 min after submit, then tail checks at 14 and 21 min.
plan :: Payout.BulkStatusCheckPlan
plan =
  Payout.BulkStatusCheckPlan
    { firstCheckDelayMinutes = 2,
      burstChecks = 3,
      burstGapMinutes = 2,
      tailChecks = 2,
      tailGapMinutes = 7,
      maxCallsPerCheck = 2,
      readCallDelaySeconds = 10,
      errorRetryDelaySeconds = 60,
      maxCallsPerDay = 12
    }

submittedAt :: UTCTime
submittedAt = Time.UTCTime (Time.fromGregorian 2026 10 1) (Time.secondsToDiffTime (10 * 3600))

-- @m@ minutes (and @s@ seconds) after submission.
minsAfterSubmit :: Integer -> Integer -> UTCTime
minsAfterSubmit m s = Time.addUTCTime (fromInteger (m * 60 + s)) submittedAt

scheduleTests :: TestTree
scheduleTests =
  testGroup
    "Check schedule"
    [ testCase "burst checks, then tail checks measured from submission" $
        checkTimes plan submittedAt @?= [minsAfterSubmit 2 0, minsAfterSubmit 4 0, minsAfterSubmit 6 0, minsAfterSubmit 14 0, minsAfterSubmit 21 0],
      testCase "first check is firstCheckDelay after submission" $
        firstCheckAt plan submittedAt @?= minsAfterSubmit 2 0,
      testCase "next check after submission is check 1" $
        nextCheck plan submittedAt 0 submittedAt @?= Just (1, minsAfterSubmit 2 0),
      testCase "next check after check 1 is check 2" $
        nextCheck plan submittedAt 1 (minsAfterSubmit 2 0) @?= Just (2, minsAfterSubmit 4 0),
      testCase "a check that is already too close or past is skipped" $
        nextCheck plan submittedAt 1 (minsAfterSubmit 5 30) @?= Just (4, minsAfterSubmit 14 0),
      testCase "no check left once the plan is used up" $
        nextCheck plan submittedAt 5 (minsAfterSubmit 21 0) @?= Nothing,
      testCase "no check left when the last one is too close" $
        nextCheck plan submittedAt 4 (minsAfterSubmit 21 0) @?= Nothing,
      -- Burst at 2, 4, 6, 8 min; tail at 6 and 9 min. The tail check at 6 overlaps the burst.
      testCase "overlapping tail checks are sorted and deduplicated" $
        checkTimes overlappingPlan submittedAt @?= [minsAfterSubmit 2 0, minsAfterSubmit 4 0, minsAfterSubmit 6 0, minsAfterSubmit 8 0, minsAfterSubmit 9 0],
      testCase "after the burst, an overlapping plan still runs its last tail check" $
        nextCheck overlappingPlan submittedAt 4 (minsAfterSubmit 8 0) @?= Just (5, minsAfterSubmit 9 0)
    ]
  where
    overlappingPlan =
      Payout.BulkStatusCheckPlan
        { firstCheckDelayMinutes = 2,
          burstChecks = 4,
          burstGapMinutes = 2,
          tailChecks = 2,
          tailGapMinutes = 3,
          maxCallsPerCheck = 2,
          readCallDelaySeconds = 10,
          errorRetryDelaySeconds = 60,
          maxCallsPerDay = 12
        }

callTests :: TestTree
callTests =
  testGroup
    "One call inside a check"
    [ testCase "\"enquire again\" calls again after readCallDelay" $
        nextStepAfterCall plan 1 NotReady @?= CallAgainIn 10,
      testCase "a failed call retries after errorRetryDelay" $
        nextStepAfterCall plan 1 CallFailed @?= CallAgainIn 60,
      testCase "an answer ends the check" $
        nextStepAfterCall plan 1 Answered @?= EndCheck,
      testCase "the check ends once maxCallsPerCheck calls are made" $
        nextStepAfterCall plan 2 NotReady @?= EndCheck
    ]

batchCompletionTests :: TestTree
batchCompletionTests =
  testGroup
    "Batch is finished only when every order has HDFC's final answer"
    [ testCase "all paid or rejected" $
        allOrdersFinal 2 [Payout.SUCCESS, Payout.FAILURE] @?= True,
      testCase "one still processing" $
        allOrdersFinal 2 [Payout.SUCCESS, Payout.PROCESSING] @?= False,
      testCase "debited is not final" $
        allOrdersFinal 1 [Payout.DEBITED] @?= False,
      testCase "awaiting approval is not final" $
        allOrdersFinal 1 [Payout.AWAITING_APPROVAL] @?= False,
      testCase "submitted with items but no orders found is not finished" $
        allOrdersFinal 3 [] @?= False,
      testCase "a batch with no items and no orders is finished" $
        allOrdersFinal 0 [] @?= True
    ]

settlementTests :: TestTree
settlementTests =
  testGroup
    "Our side is settled only when every request is final"
    [ testCase "all credited or failed" $
        unsettledCount [Just PR.CREDITED, Just PR.AUTO_PAY_FAILED] @?= 0,
      testCase "a request still processing is unsettled" $
        unsettledCount [Just PR.CREDITED, Just PR.PROCESSING] @?= 1,
      testCase "a request that cannot be found is unsettled" $
        unsettledCount [Just PR.CREDITED, Nothing] @?= 1,
      testCase "EXCLUDED is a final request status" $
        isPayoutRequestFinal PR.EXCLUDED @?= True,
      testCase "INITIATED is not a final request status" $
        isPayoutRequestFinal PR.INITIATED @?= False
    ]

-- A batch with the given status, failure code and reason, on its plan or not.
mkBatch :: DPayoutBatch.PayoutBatchStatus -> Maybe Text -> Maybe Text -> Bool -> DPayoutBatch.PayoutBatch
mkBatch status code reason onPlan =
  DPayoutBatch.PayoutBatch
    { clientRefNo = "000001",
      createdAt = submittedAt,
      excludedCount = 0,
      executionDate = Time.fromGregorian 2026 10 1,
      failureCode = code,
      failureReason = reason,
      id = Id "batch-1",
      itemCount = 2,
      merchantId = "m",
      merchantOperatingCityId = "moc",
      nextStatusCallAt = if onPlan then Just (minsAfterSubmit 2 0) else Nothing,
      origin = DPayoutBatch.SCHEDULED,
      partnerBatchRef = Nothing,
      payoutRail = DPayoutBatch.NEFT,
      payoutServiceName = "HdfcCbx",
      resolvedAt = Nothing,
      status = status,
      statusCheckCalls = 0,
      statusCheckRound = 0,
      statusNoDataReplies = 0,
      submittedAt = Just submittedAt,
      totalAmount = 0,
      updatedAt = submittedAt
    }

manualReview :: Maybe Text -> Maybe Text -> DPayoutBatch.PayoutBatch
manualReview code reason = mkBatch DPayoutBatch.MANUAL_REVIEW_REQUIRED code reason False

rejectedBatch, unrecognisedAckBatch, planRanOutBatch, submittedBatch :: DPayoutBatch.PayoutBatch
rejectedBatch = manualReview (Just "E123") (Just "HDFC rejected the batch; money still held: bad file")
unrecognisedAckBatch = manualReview (Just "2") (Just "HDFC answered the batch with an unrecognised status; money still held: x")
planRanOutBatch = manualReview (Just "CALL_FAILED") (Just "Status checks finished with items still in flight; ask HDFC about this batch")
submittedBatch = mkBatch DPayoutBatch.SUBMITTED Nothing Nothing True

-- A held bulk request in flight in the given batch.
held :: DPayoutBatch.PayoutBatch -> HoldState
held batch =
  HoldState
    { flow = BulkPayout batch,
      orderStatus = Just Payout.PROCESSING,
      requestStatus = PR.PROCESSING,
      reversalPending = False,
      holdActive = True
    }

isRefused :: ReleaseCheck -> Bool
isRefused = \case
  Refuse _ -> True
  _ -> False

holdReleaseTests :: TestTree
holdReleaseTests =
  testGroup
    "Admin hold release: money goes back only when it did not go out"
    [ testCase "Juspay/Stripe is not supported yet" $
        isRefused (checkRelease (held rejectedBatch) {flow = GatewayPayout}) @?= True,
      testCase "nothing held is a no-op" $
        checkRelease (held rejectedBatch) {holdActive = False} @?= NothingHeld,
      testCase "a credited request is refused" $
        isRefused (checkRelease (held rejectedBatch) {requestStatus = PR.CREDITED}) @?= True,
      testCase "a request marked cash pending is refused" $
        isRefused (checkRelease (held rejectedBatch) {requestStatus = PR.CASH_PENDING}) @?= True,
      testCase "a paid order is refused" $
        isRefused (checkRelease (held rejectedBatch) {orderStatus = Just Payout.SUCCESS}) @?= True,
      testCase "a debited order is refused" $
        isRefused (checkRelease (held rejectedBatch) {orderStatus = Just Payout.DEBITED}) @?= True,
      testCase "REVERSAL_PENDING is released, even in a batch on its plan" $
        checkRelease (held submittedBatch) {reversalPending = True, orderStatus = Just Payout.FAILURE, requestStatus = PR.AUTO_PAY_FAILED} @?= Release ReverseHoldOnly,
      testCase "a request with no order is released" $
        checkRelease (held submittedBatch) {orderStatus = Nothing, requestStatus = PR.INITIATED} @?= Release ReverseHoldOnly,
      testCase "a failed order in a stopped batch is released" $
        checkRelease (held planRanOutBatch) {orderStatus = Just Payout.FAILURE} @?= Release ReverseHoldOnly,
      testCase "a failed order in a batch on its plan is left to the status check" $
        isRefused (checkRelease (held submittedBatch) {orderStatus = Just Payout.FAILURE}) @?= True,
      testCase "in flight in a batch HDFC rejected: the item is failed" $
        checkRelease (held rejectedBatch) @?= Release FailInFlightBulkItem,
      testCase "in flight in a batch whose plan ran out is refused" $
        isRefused (checkRelease (held planRanOutBatch)) @?= True,
      testCase "in flight in a batch still on its plan is refused" $
        isRefused (checkRelease (held submittedBatch)) @?= True,
      testCase "HDFC never took: rejected, gateway refused, release incomplete, unsent, no record, submit failed" $
        map
          batchNeverTaken
          [ rejectedBatch,
            manualReview (Just "LEDGER_SETTLE_FAILED") (Just "HDFC gateway refused the batch (401: x), and held money could not be given back for 1 of 2 item(s): o1"),
            manualReview (Just "RELEASE_INCOMPLETE") (Just "HDFC gateway refused the batch (401: x); giving back held money."),
            manualReview (Just "UNSUBMITTED_ORDERS") (Just "Not submitted, but 1 order(s) exist under this batch"),
            manualReview (Just "NO_RECORD") (Just "Status checks finished with items still in flight; ask HDFC about this batch Last call: Partner has no record of this batch yet"),
            mkBatch DPayoutBatch.SUBMIT_FAILED (Just "401") (Just "x") False
          ]
          @?= replicate 6 True,
      testCase "HDFC has or may have it: plan ran out, duplicate, status errors, config, settle failed after answers, on plan" $
        map
          batchNeverTaken
          [ planRanOutBatch,
            manualReview (Just "DUPLICATE_FILE_REF") (Just "HDFC already has a file with reference 000001; not re-sent, money still held: dup"),
            unrecognisedAckBatch,
            manualReview (Just "STATUS_CHECK_ERROR") (Just "Status check failed 5 times in a row; last error: x"),
            manualReview (Just "CONFIG") (Just "Unknown payout service: x"),
            manualReview (Just "LEDGER_SETTLE_FAILED") (Just "HDFC gave a final answer for every item, but our wallet settle did not finish for 1 item(s)"),
            mkBatch DPayoutBatch.SUBMIT_UNKNOWN (Just "NO_RECORD") (Just "Partner has no record of this batch yet") True,
            mkBatch DPayoutBatch.CREATED Nothing Nothing True,
            submittedBatch
          ]
          @?= replicate 9 False
    ]

recheckTests :: TestTree
recheckTests =
  testGroup
    "Batch re-check: only a stopped batch HDFC has or may have"
    [ testCase "a batch whose plan ran out can be checked again" $
        checkRecheck planRanOutBatch @?= Right (),
      testCase "a batch HDFC had no record of can be checked again" $
        checkRecheck (manualReview (Just "NO_RECORD") (Just "Status checks finished")) @?= Right (),
      testCase "a batch HDFC answered with an unrecognised status can be checked again" $
        checkRecheck unrecognisedAckBatch @?= Right (),
      testCase "a batch HDFC answered with an unrecognised status is never released" $
        isRefused (checkRelease (held unrecognisedAckBatch)) @?= True,
      testCase "a batch HDFC rejected has nothing to ask" $
        isLeft (checkRecheck rejectedBatch) @?= True,
      testCase "a batch never sent has nothing to ask" $
        isLeft (checkRecheck (manualReview (Just "UNSUBMITTED_ORDERS") (Just "Not submitted"))) @?= True,
      testCase "a batch still on its plan is refused" $
        isLeft (checkRecheck submittedBatch) @?= True,
      testCase "without HDFC's batch number the job first asks whether HDFC has the file" $
        recheckStatus planRanOutBatch @?= DPayoutBatch.SUBMIT_UNKNOWN,
      testCase "with HDFC's batch number the job polls the batch" $
        recheckStatus planRanOutBatch {DPayoutBatch.partnerBatchRef = Just "B1"} @?= DPayoutBatch.SUBMITTED
    ]

closeAfterReleaseTests :: TestTree
closeAfterReleaseTests =
  testGroup
    "After a release, a stopped batch closes once nothing is open under it"
    [ testCase "HDFC never took it, every item given back: SUBMIT_FAILED" $
        closedStatusAfterRelease rejectedBatch [Payout.FAILURE, Payout.FAILURE] [Just PR.AUTO_PAY_FAILED, Just PR.AUTO_PAY_FAILED] @?= Just DPayoutBatch.SUBMIT_FAILED,
      testCase "HDFC took it, every item final and settled: COMPLETED" $
        closedStatusAfterRelease settleFailedBatch [Payout.SUCCESS, Payout.FAILURE] [Just PR.CREDITED, Just PR.AUTO_PAY_FAILED] @?= Just DPayoutBatch.COMPLETED,
      testCase "an item still in flight keeps it in manual review" $
        closedStatusAfterRelease rejectedBatch [Payout.FAILURE, Payout.PROCESSING] [Just PR.AUTO_PAY_FAILED, Just PR.PROCESSING] @?= Nothing,
      testCase "a request not settled yet keeps it in manual review" $
        closedStatusAfterRelease settleFailedBatch [Payout.SUCCESS, Payout.FAILURE] [Just PR.PROCESSING, Just PR.AUTO_PAY_FAILED] @?= Nothing,
      testCase "money still held (REVERSAL_PENDING, passed as Nothing) keeps it in manual review" $
        closedStatusAfterRelease rejectedBatch [Payout.FAILURE, Payout.FAILURE] [Just PR.AUTO_PAY_FAILED, Nothing] @?= Nothing,
      testCase "a batch not in manual review is left alone" $
        closedStatusAfterRelease submittedBatch [Payout.FAILURE, Payout.FAILURE] [Just PR.AUTO_PAY_FAILED, Just PR.AUTO_PAY_FAILED] @?= Nothing
    ]
  where
    settleFailedBatch = manualReview (Just "LEDGER_SETTLE_FAILED") (Just "HDFC gave a final answer for every item, but our wallet settle did not finish for 1 item(s)")
