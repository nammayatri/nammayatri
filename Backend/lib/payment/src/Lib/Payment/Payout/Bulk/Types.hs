-- NOTE (reviewer, remove before merge): bulk-only module, not on main (bulk lifecycle: open batch ->
--   claim -> submit -> status check -> settle). It only holds the shared types and the 'Handle' (the
--   callbacks the app gives the lib); no queries, no partner calls. Every user sits on a bulk-only path:
--     * the sweep (only inside `when (payoutServiceFlow == Payout.BulkFlow)`) and the adhoc admin API
--       (refuses any non-BulkFlow city) -> Bulk.Driver.runBulkPayoutCycle -> Bulk.Cycle.runBulkCycle;
--     * instant payout in a bulk city -> Bulk.Driver.runInstantBulkPayout -> Bulk.Cycle.openSingleBulkPayout;
--     * the BulkPayoutStatusCheck allocator job -> Bulk.StatusCheck -> Bulk.Resolve.
--   The only thing an all-flows file uses is 'localOrderCall' (Tools/Payout.createPayoutOrder), and only in
--   its `Payout.HdfcCbxConfig _` branch. A Juspay/Stripe config takes the other branch, which is main's code.

-- | Vocabulary of a bulk (file-based) payout: what a candidate is, what became of one, what the
-- partner finally said about an item, and the 'Handle' an app gives the lifecycle for the parts
-- that are its own -- which partner config a batch uses, how the status-check job is created, and
-- how an item's money is settled. No queries, no partner calls.
--
-- The split is the one the Juspay/Stripe payout flow already uses: the lib owns the payout tables
-- and the partner protocol; each app owns eligibility, the wallet lock and hold, and settlement.
module Lib.Payment.Payout.Bulk.Types
  ( BulkCandidate (..),
    BeneficiaryBank (..),
    ClaimResult (..),
    BulkClaimOutcome (..),
    BulkFinalOutcome (..),
    finalStatusOf,
    outcomeFromOrder,
    isPayoutRequestFinal,
    Handle (..),
    BulkFlow,
    toPayoutRail,
    localOrderCall,
  )
where

import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import Lib.Payment.Storage.Beam.BeamFlow (BeamFlow)

-- | What every bulk lifecycle function needs: the payout tables, Redis, and what the partner
--   interface needs to make a call.
type BulkFlow m r =
  ( MonadFlow m,
    BeamFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    MonadMask m
  )

-- NOTE (reviewer, remove before merge): one person the app's eligibility pass picked for a bulk batch
--   (built only by SharedLogic.Payout.Bulk.Eligibility). 'exclusionReason' is set when there is no bank
--   account, or its account number, IFSC or name is blank; that person is EXCLUDED. Only presence is
--   checked; HDFC validates the values. Bulk-only.

-- | One beneficiary that passed the app's read-only eligibility pass. The cycle assembles these
--   before any batch exists, so a batch can be opened already knowing who belongs to it, and every
--   row underneath it -- order or excluded request -- carries its batchId from birth.
--   @b@ is the app's own beneficiary (the driver app passes its Person).
data BulkCandidate b = BulkCandidate
  { beneficiary :: b,
    -- | What this beneficiary would be paid, as of the eligibility pass. The app re-reads it under
    --   its own lock before anything is claimed; this copy only sizes the batch.
    amount :: HighPrecMoney,
    -- | Why this beneficiary cannot be paid although they belong to the batch: no bank account on
    --   file, or a blank account number, IFSC or name. 'Nothing' means payable. The text is what the
    --   excluded worklist shows, so it is the operator-facing message.
    exclusionReason :: Maybe Text
  }

-- | What the partner is sent for one item, from the app's own bank account table.
data BeneficiaryBank = BeneficiaryBank
  { accountNumber :: Text,
    ifscCode :: Text,
    holderName :: Text
  }

-- NOTE (reviewer, remove before merge): what the app's claim (SharedLogic.Payout.Bulk.Claim) returns per
--   person; runBulkCycle turns it into BulkClaimOutcome (the adhoc API shows it per person; the sweep
--   ignores it). Claimed = hold and order written by the shared wallet payout (initiateWalletPayoutWith,
--   hold first). Excluded = an EXCLUDED request, nothing held. Dropped = not claimed in this run, with the
--   reason; nothing held. Bulk-only types.

-- | What the app's claim did with one candidate.
data ClaimResult
  = -- | Claimed and ready to submit: the order it created and the bank details its checks passed on.
    Claimed DPayoutOrder.PayoutOrder BeneficiaryBank
  | -- | Excluded for want of usable bank details: an EXCLUDED payout_request, nothing held.
    Excluded (Id PR.PayoutRequest)
  | -- | Not claimed in this run, and why: the claim-time re-check said no, the wallet lock stayed
    --   busy, or the claim failed. Nothing is left held.
    Dropped Text

-- | What became of one beneficiary in a bulk payout cycle. The scheduled sweep ignores this; the
--   adhoc flow reports it back per person.
data BulkClaimOutcome
  = ClaimSubmitted DPayoutOrder.PayoutOrder
  | ClaimExcluded Text
  | ClaimDropped Text

-- NOTE (reviewer, remove before merge): HDFC's final answer for one item, handed to the app's settle (the
--   driver app's settleBulkItem). Built in Bulk.Resolve, in Bulk.Submit (gateway refusal) and by
--   outcomeFromOrder below. BulkRejected's 4th field keeps HDFC's bank reference for a rejected row too.
--   Bulk-only.

-- | The partner's final answer for one item, as the inquiry (or a refused submission) reported it.
data BulkFinalOutcome
  = -- | Paid: the settlement reference and its type, the settlement status (rbistatus mirror),
    --   and the partner's code and note.
    BulkPaid Text Payout.SettlementRefType (Maybe Payout.TransferStatus) (Maybe Text) (Maybe Text)
  | -- | Refused or returned: the partner's code and reason, the settlement status
    --   (TRANSFER_FAILED for a post-debit return, Nothing for a validation rejection), and the
    --   partner's reference for the item if the row carried one.
    BulkRejected (Maybe Text) (Maybe Text) (Maybe Payout.TransferStatus) (Maybe (Text, Payout.SettlementRefType))

finalStatusOf :: BulkFinalOutcome -> Payout.PayoutOrderStatus
finalStatusOf = \case
  BulkPaid {} -> Payout.SUCCESS
  BulkRejected {} -> Payout.FAILURE

-- NOTE (reviewer, remove before merge): rebuilds HDFC's answer from what we saved on an order that is
--   already final, so the status check can run our settle again without asking HDFC. This is for an
--   earlier settle that wrote the order but not the request (e.g. a ledger error). Nothing is re-sent to
--   HDFC. Used only by Bulk.Resolve.applyItemOutcome.

-- | The partner's final answer, as stored on an order that is already final. Used to run an
--   order's settle again (the status check's re-run).
outcomeFromOrder :: DPayoutOrder.PayoutOrder -> BulkFinalOutcome
outcomeFromOrder o
  | o.status == Payout.SUCCESS =
    BulkPaid (fromMaybe "" o.settlementRef) (fromMaybe Payout.PARTNER_REF o.settlementRefType) o.transferStatus o.responseCode o.responseMessage
  | otherwise = BulkRejected o.responseCode o.responseMessage o.transferStatus savedRef
  where
    savedRef = case (o.settlementRef, o.settlementRefType) of
      (Just ref, Just refType) -> Just (ref, refType)
      _ -> Nothing

-- NOTE (reviewer, remove before merge): the "settled on our side" test. The app writes the request final
--   only after its ledger step, so a final order with a non-final request means our settle did not finish.
--   Bulk.Resolve uses it to pick between re-running the settle, COMPLETED and MANUAL_REVIEW_REQUIRED.
--   CASH_PAID / CASH_PENDING count as final (an admin closed the request another way). Bulk-only.

-- | A request the settlement has finished with. Written only after the app's ledger step, so a
--   request that is not final under a final order means that order's settle has not completed.
isPayoutRequestFinal :: PR.PayoutRequestStatus -> Bool
isPayoutRequestFinal = (`elem` [PR.CREDITED, PR.AUTO_PAY_FAILED, PR.FAILED, PR.CANCELLED, PR.CASH_PAID, PR.CASH_PENDING])

-- NOTE (reviewer, remove before merge): the callbacks the driver app gives the lib (built in
--   SharedLogic.Payout.Bulk.Driver.driverBulkHandle). Same pattern as main's Juspay/Stripe
--   Lib.Payment.Payout.StatusCheck.Handle, but a separate type: main's Handle is not changed.
--   * keyPrefix: "" in the driver app, so the lib's Redis keys (locks, file-number counter) get no extra prefix.
--   * createCityStatusCheckJob: one status-check job per city (createJobInWithCheck, job data
--     {merchantId, merchantOperatingCityId}, upper limit 1).
--   * settleItem: settles through the shared UIPayout.payoutSettlementActionWith, the same ledger logic as
--     Juspay/Stripe. It raises when the settle did not run, and the lib then keeps the item open.

-- | The parts of the lifecycle that belong to the app. Same idea as
--   'Lib.Payment.Payout.StatusCheck.Handle' for the Juspay/Stripe status check.
data Handle m = Handle
  { -- | Put in front of every Redis key the lifecycle uses, so apps sharing a Redis do not see
    --   each other's batch locks, job-create locks or file-number counters.
    keyPrefix :: Text,
    -- | The partner config a batch was submitted through. 'Left' when it cannot be resolved; the
    --   batch is then handed to a human rather than guessed at.
    partnerFor :: DPayoutBatch.PayoutBatch -> m (Either Text Payout.PayoutServiceConfig),
    -- | Create one city's status-check job, unless a pending one already exists for that city (the
    --   job type and the scheduler are the app's). Arguments: merchantId, merchantOperatingCityId.
    createCityStatusCheckJob :: Text -> Text -> m (),
    -- | Settle one item the partner has finished with: write its order and request, and move the
    --   money (hold to paid out, or back to the wallet). Raises when it could not complete; the lib
    --   keeps the item pending. Arguments: the batch's merchantOperatingCityId, the order, its
    --   request, and the partner's answer.
    settleItem :: Text -> DPayoutOrder.PayoutOrder -> Maybe PR.PayoutRequest -> BulkFinalOutcome -> m ()
  }

toPayoutRail :: DPayoutBatch.PayoutBatchRail -> Payout.PayoutRail
toPayoutRail = \case
  DPayoutBatch.NEFT -> Payout.RailNEFT
  DPayoutBatch.RTGS -> Payout.RailRTGS
  DPayoutBatch.IMPS -> Payout.RailIMPS
  DPayoutBatch.A2A -> Payout.RailA2A

-- NOTE (reviewer, remove before merge): the one function here that an all-flows caller uses.
--   Tools/Payout.createPayoutOrder calls it only for an HdfcCbxConfig; every other config takes main's
--   runWithServiceConfigAndName call, unchanged. So Juspay/Stripe never reach this function.

-- | The 'payoutCall' for a bulk rail. A bulk partner has no single-order API: the order is created
--   locally, INITIATED, and reaches the partner later in a batch file.
localOrderCall :: (Monad m) => DPayment.CreatePayoutServiceReq -> m Payout.CreatePayoutOrderResp
localOrderCall serviceReq =
  pure
    Payout.CreatePayoutOrderResp
      { orderId = serviceReq.orderId,
        status = Payout.INITIATED,
        -- No settlement statement exists yet: the column is the partner's settlement mirror, and
        -- the partner has not been called.
        transferStatus = Nothing,
        orderType = Just serviceReq.orderType,
        transferId = Nothing,
        idAssignedByServiceProvider = Nothing,
        udf1 = Nothing,
        udf2 = Nothing,
        udf3 = Nothing,
        udf4 = Nothing,
        udf5 = Nothing,
        amount = serviceReq.amount,
        refunds = Nothing,
        payments = Nothing,
        fulfillments = Nothing,
        customerId = Just serviceReq.customerId,
        merchantTopUpAmount = Nothing
      }
