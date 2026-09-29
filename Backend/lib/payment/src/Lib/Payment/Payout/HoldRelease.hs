-- | When an admin may give back held payout money, and when a stopped bulk batch may be re-checked.
--   Money goes back only when we know it did not go out; otherwise the person is paid twice.
module Lib.Payment.Payout.HoldRelease
  ( PayoutFlow (..),
    HoldState (..),
    ReleaseHow (..),
    ReleaseCheck (..),
    checkRelease,
    batchNeverTaken,
    batchOnPlan,
    checkRecheck,
    recheckStatus,
    closedStatusAfterRelease,
    releasedByAdminPrefix,
  )
where

import qualified Data.Text as T
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import Lib.Payment.Payout.Bulk.Resolve (allOrdersFinal, unsettledCount)
import Lib.Payment.Payout.Bulk.Submit (batchRejectedPrefix, gatewayRefusedPrefix)

-- | Which payout flow a request went through.
data PayoutFlow
  = -- | HDFC bulk: the request belongs to this batch.
    BulkPayout DPayoutBatch.PayoutBatch
  | -- | Juspay / Stripe: one order per request, sent to the partner on its own.
    GatewayPayout

-- | What the rows say about one request's hold.
data HoldState = HoldState
  { flow :: PayoutFlow,
    -- | The request's order; Nothing when no order was written.
    orderStatus :: Maybe Payout.PayoutOrderStatus,
    requestStatus :: PR.PayoutRequestStatus,
    -- | The request was never sent and giving its money back failed (failure_reason
    --   "REVERSAL_PENDING: ...").
    reversalPending :: Bool,
    -- | The ledger still has a hold for this request that is not reversed.
    holdActive :: Bool
  }

-- | How the money goes back.
data ReleaseHow
  = -- | Only the ledger step: the hold is reversed. For an order that is already final, or none.
    ReverseHoldOnly
  | -- | In flight in a batch HDFC never took: fail the order and request and reverse the hold,
    --   as a gateway refusal does.
    FailInFlightBulkItem
  deriving (Show, Eq)

data ReleaseCheck
  = Release ReleaseHow
  | Refuse Text
  | NothingHeld
  deriving (Show, Eq)

-- | Start of the request's failure reason after an admin gave its money back.
releasedByAdminPrefix :: Text
releasedByAdminPrefix = "RELEASED_BY_ADMIN: "

-- | Whether one request's held money may be given back.
checkRelease :: HoldState -> ReleaseCheck
checkRelease s = case s.flow of
  GatewayPayout -> checkGateway s
  BulkPayout batch -> checkBulk batch s

-- TODO (Juspay/Stripe): allow 'Release ReverseHoldOnly' for an active hold whose request is
--   REVERSAL_PENDING or whose order is FAILURE (a Stripe refusal leaves the request PROCESSING).
--   Keep refusing paid requests, SUCCESS or in-flight orders, and a request with no order.
checkGateway :: HoldState -> ReleaseCheck
checkGateway _ = Refuse "Releasing held money is not supported yet for Juspay/Stripe payouts"

checkBulk :: DPayoutBatch.PayoutBatch -> HoldState -> ReleaseCheck
checkBulk batch s
  | not s.holdActive = NothingHeld
  | s.requestStatus `elem` [PR.CREDITED, PR.CASH_PAID, PR.CASH_PENDING] =
    Refuse ("The payout is " <> show s.requestStatus <> "; giving the money back would pay twice")
  | s.orderStatus `elem` [Just Payout.SUCCESS, Just Payout.DEBITED] =
    Refuse "HDFC paid this item, or the money has already left HDFC"
  -- Never sent (our give-back failed, or no order exists): only an order in a sent file reaches HDFC.
  | s.reversalPending || isNothing s.orderStatus = Release ReverseHoldOnly
  -- HDFC rejected the item and only our reversal is missing. While the batch is on its plan, the
  -- next inquiry retries that reversal.
  | s.orderStatus == Just Payout.FAILURE =
    if batchOnPlan batch
      then Refuse "The status check will retry this reversal; try again once the batch has stopped"
      else Release ReverseHoldOnly
  | batchNeverTaken batch = Release FailInFlightBulkItem
  | otherwise = Refuse "HDFC has or may have this file; run the batch re-check first"

-- | The status-check job still owes this batch a call.
batchOnPlan :: DPayoutBatch.PayoutBatch -> Bool
batchOnPlan batch = isJust batch.nextStatusCallAt

-- | HDFC never took this batch's file, so nothing in it was paid: refused by HDFC or its gateway,
--   never sent, or no record of it for the whole plan (ops confirm with HDFC before releasing).
batchNeverTaken :: DPayoutBatch.PayoutBatch -> Bool
batchNeverTaken batch = case batch.status of
  DPayoutBatch.SUBMIT_FAILED -> True
  DPayoutBatch.MANUAL_REVIEW_REQUIRED ->
    batch.failureCode `elem` map Just ["RELEASE_INCOMPLETE", "UNSUBMITTED_ORDERS", "NO_RECORD"]
      || any (\prefix -> maybe False (prefix `T.isPrefixOf`) batch.failureReason) [batchRejectedPrefix, gatewayRefusedPrefix]
  _ -> False

-- | Whether a batch may be checked again. Only a stopped batch HDFC has, or may have: a batch HDFC
--   never took has nothing to ask about, except one HDFC had no record of, which is asked again.
checkRecheck :: DPayoutBatch.PayoutBatch -> Either Text ()
checkRecheck batch
  | batch.status /= DPayoutBatch.MANUAL_REVIEW_REQUIRED =
    Left ("Only a batch in MANUAL_REVIEW_REQUIRED can be checked again; this one is " <> show batch.status)
  | batchNeverTaken batch && batch.failureCode /= Just "NO_RECORD" =
    Left "HDFC never took this batch, so there is nothing to ask; release its held money instead"
  | otherwise = Right ()

-- | The status a re-checked batch goes back to. Without HDFC's batch number the job first asks
--   whether HDFC has our file; with it, the job asks how the batch is doing.
recheckStatus :: DPayoutBatch.PayoutBatch -> DPayoutBatch.PayoutBatchStatus
recheckStatus batch
  | isNothing batch.partnerBatchRef = DPayoutBatch.SUBMIT_UNKNOWN
  | otherwise = DPayoutBatch.SUBMITTED

-- | The status a stopped batch closes in once an admin release leaves nothing open, or Nothing.
--   Same rule as the status check; pass a request still held (e.g. REVERSAL_PENDING) as Nothing.
closedStatusAfterRelease ::
  DPayoutBatch.PayoutBatch ->
  [Payout.PayoutOrderStatus] -> -- every order of the batch
  [Maybe PR.PayoutRequestStatus] -> -- each order's request
  Maybe DPayoutBatch.PayoutBatchStatus
closedStatusAfterRelease batch orderStatuses requestStatuses
  | batch.status /= DPayoutBatch.MANUAL_REVIEW_REQUIRED = Nothing
  | not (allOrdersFinal batch.itemCount orderStatuses) || unsettledCount requestStatuses > 0 = Nothing
  | batchNeverTaken batch = Just DPayoutBatch.SUBMIT_FAILED
  | otherwise = Just DPayoutBatch.COMPLETED
