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
    ParkedTransfer (..),
    ManualDecision (..),
    listParkedTransfers,
    loadParkedTransfer,
    closeOrderByDecision,
  )
where

import qualified Data.Text as T
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (MonadFlow, fromMaybeM, throwError)
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.Transfer as DT
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Finance.Storage.Queries.Account as QFAccount
import qualified Lib.Finance.Storage.Queries.Transfer as QT
import qualified Lib.Finance.Storage.Queries.TransferExtra as QTE
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as PayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import Lib.Payment.Payout.Bulk.Resolve (allOrdersFinal, unsettledCount)
import Lib.Payment.Payout.Bulk.Submit (batchRejectedPrefix, gatewayRefusedPrefix)
import Lib.Payment.Payout.StatusCheck (isPayoutOrderSuccess, isTerminalPayoutOrderStatus)
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder

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

-- ---------------------------------------------------------------------------
-- Transfers the finance kernel parked as NEEDS_REVIEW (money may be with the partner)
-- ---------------------------------------------------------------------------

-- | A parked transfer with the owner of its account, for the ops list.
data ParkedTransfer = ParkedTransfer
  { transfer :: DT.Transfer,
    ownerType :: Maybe Text,
    ownerId :: Maybe Text
  }

-- | One page of the merchant city's transfers awaiting review, most recently touched first.
listParkedTransfers ::
  (FinanceBeamFlow.BeamFlow m r) =>
  Text -> -- merchant id
  Text -> -- merchant operating city id
  Int ->
  Int ->
  m [ParkedTransfer]
listParkedTransfers merchantId cityId limit offset = do
  rows <- QTE.findAllByMerchantAndStatus merchantId (Just cityId) [DT.NEEDS_REVIEW] (Just limit) (Just offset)
  forM rows $ \t -> do
    mbAccount <- QFAccount.findById t.accountId
    pure
      ParkedTransfer
        { transfer = t,
          ownerType = show <$> (mbAccount >>= (.counterpartyType)),
          ownerId = mbAccount >>= (.counterpartyId)
        }

-- | The transfer to act on: it must belong to this merchant city and still be awaiting review.
loadParkedTransfer ::
  (FinanceBeamFlow.BeamFlow m r, MonadFlow m) =>
  Text -> -- merchant id
  Text -> -- merchant operating city id
  Text -> -- transfer id
  m DT.Transfer
loadParkedTransfer merchantId cityId transferId =
  QT.findById (Id transferId) >>= \case
    Just t | t.merchantId == merchantId && t.merchantOperatingCityId == cityId -> do
      unless (t.status == DT.NEEDS_REVIEW) $
        throwError $ InvalidRequest ("Transfer " <> transferId <> " is " <> show t.status <> ", not awaiting review")
      pure t
    _ -> throwError $ InvalidRequest ("Transfer not found: " <> transferId)

-- | What ops decided about a parked payout.
data ManualDecision = ManualSucceeded | ManualFailed
  deriving (Eq, Show)

-- | Record ops' decision on the payout order itself (who and why) and on its payout request, and return the
--   status the normal settlement should now apply. An order the partner already ended is left as it is when
--   the decision agrees with it, so a half-finished settlement can be retried. Two decisions overrule it:
--   "paid" over a partner failure that arrived after the money went out (the order was sent, so ops may know
--   better), and nothing else. "Failed" over a paid order is refused, and so is "paid" over an order that
--   was never sent, because the ledger would then move money the partner did not.
closeOrderByDecision ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r, Finance.HasActorInfo m r, MonadFlow m) =>
  PayoutOrder.PayoutOrder ->
  ManualDecision ->
  Text -> -- who
  Text -> -- why
  m Payout.PayoutOrderStatus
closeOrderByDecision stale decision actor reason = do
  order <- QPayoutOrder.findByOrderId stale.orderId >>= fromMaybeM (InvalidRequest ("Payout order not found: " <> stale.orderId))
  let (status, decidedPaid) = case decision of
        ManualSucceeded -> (Payout.SUCCESS, True)
        ManualFailed -> (Payout.FAILURE, False)
      neverSent = order.responseCode `elem` [Just "NOT_SENT", Just "E09"]
      close = do
        QPayoutOrder.updateBulkSettled status order.transferStatus order.settlementRef order.settlementRefType (Just "MANUAL") (Just (T.take 500 ("Resolved manually by " <> actor <> ": " <> reason))) order.id
        DPayment.updatePayoutRequestStatusFromOrder order status
  if
      | not (isTerminalPayoutOrderStatus order.status) -> close
      | isPayoutOrderSuccess order.status == decidedPaid -> pure ()
      | decidedPaid && not neverSent -> close
      | otherwise ->
        throwError . InvalidRequest $
          "The payout order is already " <> show order.status
            <> (if decidedPaid then " and it was never sent to the partner, so it cannot be marked paid." else "; it was paid, so it cannot be marked failed here. Mark it succeeded to accept it as paid.")
  pure status
