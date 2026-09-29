-- | Types for bulk payouts. The lib owns the payout tables and partner calls; the app supplies
-- eligibility, the wallet hold and settlement through 'Handle'.
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

-- | Constraints for the bulk lifecycle: payout tables, Redis and partner calls.
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

-- | A beneficiary that passed the app's eligibility pass. Gathered before the batch is opened, so
--   every row under the batch carries its batchId from the start. @b@ is the app's beneficiary.
data BulkCandidate b = BulkCandidate
  { beneficiary :: b,
    -- | Only sizes the batch; the app re-reads the amount under its lock before claiming.
    amount :: HighPrecMoney,
    -- | Why they cannot be paid (missing bank details); 'Nothing' means payable. Shown to
    --   operators on the excluded worklist.
    exclusionReason :: Maybe Text
  }

-- | Bank details sent to the partner for one item, from the app's bank account table.
data BeneficiaryBank = BeneficiaryBank
  { accountNumber :: Text,
    ifscCode :: Text,
    holderName :: Text
  }

-- | What the app's claim did with one candidate.
data ClaimResult
  = -- | Ready to submit, with the bank details that passed the checks.
    Claimed DPayoutOrder.PayoutOrder BeneficiaryBank
  | -- | No usable bank details: an EXCLUDED request was written and nothing is held.
    Excluded (Id PR.PayoutRequest) Text
  | -- | Not claimed this run (re-check said no, lock busy, or claim error); nothing is held.
    Dropped Text

-- | What became of one beneficiary in a cycle. Only the adhoc flow reports it back.
data BulkClaimOutcome
  = ClaimSubmitted DPayoutOrder.PayoutOrder
  | ClaimExcluded Text
  | ClaimDropped Text

-- | The partner's final answer for one item, from an inquiry or a refused submission.
data BulkFinalOutcome
  = -- | Paid: settlement ref and its type, settlement status (rbistatus), partner code and note.
    BulkPaid (Maybe Text) Payout.SettlementRefType (Maybe Payout.TransferStatus) (Maybe Text) (Maybe Text)
  | -- | Refused or returned: partner code and reason, settlement status (TRANSFER_FAILED for a
    --   post-debit return, Nothing for a validation rejection), and the item's partner ref if any.
    BulkRejected (Maybe Text) (Maybe Text) (Maybe Payout.TransferStatus) (Maybe (Text, Payout.SettlementRefType))

finalStatusOf :: BulkFinalOutcome -> Payout.PayoutOrderStatus
finalStatusOf = \case
  BulkPaid {} -> Payout.SUCCESS
  BulkRejected {} -> Payout.FAILURE

-- | The final answer stored on an already-final order, so its settle can be run again.
outcomeFromOrder :: DPayoutOrder.PayoutOrder -> BulkFinalOutcome
outcomeFromOrder o
  | o.status == Payout.SUCCESS =
    BulkPaid o.settlementRef (fromMaybe Payout.PARTNER_REF o.settlementRefType) o.transferStatus o.responseCode o.responseMessage
  | otherwise = BulkRejected o.responseCode o.responseMessage o.transferStatus savedRef
  where
    savedRef = case (o.settlementRef, o.settlementRefType) of
      (Just ref, Just refType) -> Just (ref, refType)
      _ -> Nothing

-- | Final statuses are written only after the app's ledger step, so a non-final request under a
--   final order means its settle did not complete.
isPayoutRequestFinal :: PR.PayoutRequestStatus -> Bool
isPayoutRequestFinal = (`elem` [PR.CREDITED, PR.AUTO_PAY_FAILED, PR.FAILED, PR.CANCELLED, PR.CASH_PAID, PR.CASH_PENDING, PR.EXCLUDED])

-- | The parts of the bulk lifecycle that the app supplies.
data Handle m = Handle
  { -- | Prefix for every Redis key, so apps sharing a Redis keep separate locks and counters.
    keyPrefix :: Text,
    -- | The partner config the batch was submitted through. 'Left' sends the batch to manual
    --   review instead of guessing.
    partnerFor :: DPayoutBatch.PayoutBatch -> m (Either Text Payout.PayoutServiceConfig),
    -- | Create the city's status-check job unless one is already pending.
    --   Args: merchantId, merchantOperatingCityId.
    createCityStatusCheckJob :: Text -> Text -> m (),
    -- | Write the final order and request, and move the money (hold to paid out, or back to the
    --   wallet). Throws if it cannot finish. First arg: the batch's merchantOperatingCityId.
    settleItem :: Text -> DPayoutOrder.PayoutOrder -> Maybe PR.PayoutRequest -> BulkFinalOutcome -> m ()
  }

toPayoutRail :: DPayoutBatch.PayoutBatchRail -> Payout.PayoutRail
toPayoutRail = \case
  DPayoutBatch.NEFT -> Payout.RailNEFT
  DPayoutBatch.RTGS -> Payout.RailRTGS
  DPayoutBatch.IMPS -> Payout.RailIMPS
  DPayoutBatch.A2A -> Payout.RailA2A

-- | The 'payoutCall' for a bulk rail: the order is only created locally as INITIATED and reaches
--   the partner later in a batch.
localOrderCall :: (Monad m) => DPayment.CreatePayoutServiceReq -> m Payout.CreatePayoutOrderResp
localOrderCall serviceReq =
  pure
    Payout.CreatePayoutOrderResp
      { orderId = serviceReq.orderId,
        status = Payout.INITIATED,
        -- The partner has not been called yet, so there is no settlement status to mirror.
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
