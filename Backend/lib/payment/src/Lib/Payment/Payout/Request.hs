{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.Payout.Request
  ( PayoutRequest (..),
    PayoutRequestStatus (..),
    PayoutSubmission (..),
    PayoutResult (..),
    -- NOTE (reviewer, remove before merge): exports vs main. ExecutionOutcome takes the place of
    --   main's PayoutExecutionResult, which nothing outside this file used. buildPayoutRequest and
    --   executePayoutRequestWithOutcome are new exports so the driver wallet payout can do
    --   request -> hold -> partner call. The new imports are only for batchId and the bulk-only
    --   rejection check.
    ExecutionOutcome (..),
    buildPayoutRequest,
    createPayoutRequest,
    submitPayoutRequest,
    executePayoutRequest,
    executePayoutRequestWithOutcome,
    isPayoutExecutable,
    ensurePayoutExecutable,
    getPayoutRequestById,
    getPayoutRequestByEntity,
    updateStatusWithHistoryById,
    createInitialHistory,
    markCashPending,
    markCashPaid,
    cancelPayoutWithin,
    retryPayoutWith,
    toPaymentState,
    getStatusMessage,
    runPayoutUnderLock,
    stashPayoutLedgerEntryIds,
    getPayoutLedgerEntryIds,
    clearPayoutLedgerEntryIds,
  )
where

import Control.Applicative ((<|>))
import Data.Time.Clock (addUTCTime)
import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.Payout.Interface as Payout
import qualified Kernel.External.Payout.Interface.Types as IPayout
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error (ExternalAPICallError (..), GenericError (InvalidRequest))
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (CacheFlow, Currency, HighPrecMoney, MonadFlow, fromMaybeM, generateGUID, getCurrentTime, logDebug, logError, logInfo, throwError)
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Ledger.Service as LedgerService
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DCommon
import Lib.Payment.Domain.Types.PayoutBatch (PayoutBatch)
import qualified Lib.Payment.Domain.Types.PayoutOrder as PayoutOrder
import Lib.Payment.Domain.Types.PayoutRequest
import Lib.Payment.Payout.RequestStatus (getStatusMessage, recordHistory, toPaymentState, updatePayoutRequestStatusWithHistory)
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import Servant.Client (ClientError (..))

-- | Flat data record supplied by the domain to request a payout.
--   The lib uses this to construct both the PayoutRequest and CreatePayoutOrderReq.
data PayoutSubmission = PayoutSubmission
  { beneficiaryId :: Text,
    -- NOTE (reviewer, remove before merge): batchId is copied to payout_request.batch_id and
    --   payout_order.batch_id. Only the HDFC bulk callers set it (the bulk claim and the instant
    --   payout in a bulk city, Bulk.Driver.runInstantBulkPayout). Juspay/Stripe wallet payouts, rider
    --   cashback and Registration pass Nothing, so those rows keep the new columns NULL.

    -- | Set only by the HDFC bulk flow, where a payout belongs to a partner batch. Every other
    --   payout (Juspay, Stripe, rider cashback, registration refund) has no batch and passes Nothing.
    batchId :: Maybe (Id PayoutBatch),
    entityName :: DCommon.EntityName,
    entityId :: Text,
    entityRefId :: Maybe Text,
    amount :: HighPrecMoney,
    currency :: Currency,
    payoutServiceFlow :: Payout.PayoutServiceFlow,
    payoutFee :: Maybe HighPrecMoney,
    transferAmount :: Maybe HighPrecMoney, -- explicit merchant→driver transfer amount (Nothing = use amount)
    merchantId :: Text,
    merchantOpCityId :: Text,
    city :: Text,
    vpa :: Maybe Text,
    bankName :: Maybe Text,
    bankAccountLast4 :: Maybe Text,
    customerName :: Maybe Text,
    customerPhone :: Maybe Text,
    customerEmail :: Maybe Text,
    remark :: Text,
    orderType :: Text,
    scheduledAt :: Maybe UTCTime,
    payoutType :: Maybe PayoutType,
    coverageFrom :: Maybe UTCTime,
    coverageTo :: Maybe UTCTime,
    ledgerEntryIds :: [Text]
  }
  deriving (Show, Generic)

-- NOTE (reviewer, remove before merge): PayoutAmbiguous is the one new constructor. Only BulkFlow
--   (HDFC) returns it, when the error is not a clear rejection (see executePayoutRequestInternal).
--   A Juspay/Stripe error is still PayoutFailed with main's text. No caller of submitPayoutRequest
--   passes BulkFlow today (rider cashback is always JuspayFlow; the dashboard registration refund
--   refuses a bulk city), so it is not produced yet. Rider cashback's case arm for it only keeps the
--   match complete; Registration passes the result through.

-- | Result of a payout submission or execution.
--   PayoutFailed is a *confirmed* rejection (bank responded; nothing was paid).
--   PayoutAmbiguous: the order call failed without a clear rejection; the request is left
--   PROCESSING. Only BulkFlow returns it, and for bulk that call is local (Bulk.localOrderCall
--   writes the order; no bank is contacted), so nothing was sent.
data PayoutResult
  = PayoutInitiated PayoutRequest PayoutOrder.PayoutOrder
  | PayoutProcessing PayoutRequest PayoutRequestStatus
  | PayoutFailed PayoutRequest Text
  | PayoutAmbiguous PayoutRequest Text

-- ---------------------------------------------------------------------------
-- CRUD
-- ---------------------------------------------------------------------------
createPayoutRequest ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  PayoutRequest ->
  m ()
createPayoutRequest payoutRequest = do
  QPR.create payoutRequest
  createInitialHistory payoutRequest

getPayoutRequestById ::
  (PaymentBeamFlow.BeamFlow m r) =>
  Id PayoutRequest ->
  m (Maybe PayoutRequest)
getPayoutRequestById = QPR.findById

getPayoutRequestByEntity ::
  (PaymentBeamFlow.BeamFlow m r) =>
  Maybe DCommon.EntityName ->
  Text ->
  m (Maybe PayoutRequest)
getPayoutRequestByEntity entityName entityId = QPR.findByEntity entityId entityName

-- ---------------------------------------------------------------------------
-- Status operations
-- ---------------------------------------------------------------------------

-- | Check if a PayoutRequest is in a state that allows execution.
--   Only INITIATED payouts can be executed.
isPayoutExecutable :: PayoutRequest -> Bool
isPayoutExecutable pr = pr.status == INITIATED

-- | Ensure a PayoutRequest is executable, returning an error message if not.
--   This encodes the status idempotency logic that every payout caller needs.
ensurePayoutExecutable :: (MonadFlow m) => PayoutRequest -> m ()
ensurePayoutExecutable pr =
  unless (isPayoutExecutable pr) $
    throwError $ InvalidRequest $ "PayoutRequest " <> pr.id.getId <> " is not executable (status: " <> show pr.status <> ")"

updateStatusWithHistoryById ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  PayoutRequestStatus ->
  Maybe Text ->
  PayoutRequest ->
  m ()
updateStatusWithHistoryById = updatePayoutRequestStatusWithHistory

createInitialHistory ::
  (FinanceBeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  PayoutRequest ->
  m ()
createInitialHistory payoutRequest = do
  recordHistory Nothing payoutRequest.status (Just $ getStatusMessage payoutRequest.status) payoutRequest

markCashPending ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  Text ->
  Text ->
  Maybe Text ->
  PayoutRequest ->
  m ()
markCashPending agentId agentName mbMessage payoutRequest = do
  now <- getCurrentTime
  QPR.updateCashDetailsById (Just agentId) (Just agentName) (Just now) payoutRequest.id
  updateStatusWithHistoryById CASH_PENDING (mbMessage <|> Just ("Cash Pending marked by " <> agentName)) payoutRequest

markCashPaid ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  Text ->
  Text ->
  Maybe Text ->
  PayoutRequest ->
  m ()
markCashPaid agentId agentName mbMessage payoutRequest = do
  now <- getCurrentTime
  QPR.updateCashDetailsById (Just agentId) (Just agentName) (Just now) payoutRequest.id
  updateStatusWithHistoryById CASH_PAID (mbMessage <|> Just ("Cash Paid marked by " <> agentName)) payoutRequest

cancelPayoutWithin ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  NominalDiffTime ->
  Text ->
  PayoutRequest ->
  m ()
cancelPayoutWithin window reason payoutRequest = do
  now <- getCurrentTime
  let cutoffTime = addUTCTime window payoutRequest.createdAt
  unless (payoutRequest.status == INITIATED) $
    throwError $ InvalidRequest "Can only cancel INITIATED payouts"
  unless (now < cutoffTime) $
    throwError $ InvalidRequest "Cancel window expired"
  QPR.updateStatusWithReasonById CANCELLED (Just reason) payoutRequest.id
  recordHistory (Just payoutRequest.status) CANCELLED (Just $ "Cancelled: " <> reason) payoutRequest

retryPayoutWith ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  (PayoutRequestStatus -> Bool) ->
  (PayoutRequest -> m ()) ->
  PayoutRequest ->
  m ()
retryPayoutWith canRetry executePayout payoutRequest = do
  unless (canRetry payoutRequest.status) $
    throwError $ InvalidRequest "Payout is not eligible for retry"
  let nextRetryCount = Just $ fromMaybe 0 payoutRequest.retryCount + 1
  QPR.updateRetryCountById nextRetryCount payoutRequest.id
  updateStatusWithHistoryById RETRYING (Just "Admin initiated retry...") payoutRequest
  executePayout payoutRequest

-- ---------------------------------------------------------------------------
-- Payout execution
-- ---------------------------------------------------------------------------

-- | One payout attempt for a payee under a wait lock. Both the amount finder and the initiate run
--   inside the lock, so a concurrent attempt only computes its amount after this one has posted
--   its hold and therefore sees the reduced balance.
runPayoutUnderLock ::
  (CacheFlow m r, MonadFlow m) =>
  Text -> -- lock key
  Int -> -- lock ttl (seconds)
  m (Maybe a) -> -- find what to pay; Nothing = nothing to pay now
  (a -> m ()) -> -- initiate the payout
  m ()
runPayoutUnderLock lockKey lockTtl findPayoutAmount initiatePayout =
  Redis.withWaitAndLockMasterCloudCrossAppRedis "payout" "waitForPayoutLock" lockKey lockTtl 100 (findPayoutAmount >>= (`whenJust` initiatePayout))

makePayoutEntryIdsKey :: Text -> Text
makePayoutEntryIdsKey payoutRequestId = "payout-entry-ids:" <> payoutRequestId

payoutEntryIdsTtl :: Int
payoutEntryIdsTtl = 30 * 86400

-- | Stash the ledger entry ids reserved for a payout so the webhook can settle or
--   release them once the payout reaches a terminal status.
stashPayoutLedgerEntryIds :: (CacheFlow m r) => Text -> [Text] -> m ()
stashPayoutLedgerEntryIds payoutRequestId entryIds =
  unless (null entryIds) $ Redis.setExp (makePayoutEntryIdsKey payoutRequestId) entryIds payoutEntryIdsTtl

-- | Redis first, then the ids persisted on the PayoutRequest (payouts created before the
--   stash flow), and finally the ledger rows stamped with this PayoutRequest id as settlementId
--   (reserved as PROCESSING at initiate), so a lost stash can still be settled or released.
getPayoutLedgerEntryIds :: (CacheFlow m r, FinanceBeamFlow.BeamFlow m r) => PayoutRequest -> m [Text]
getPayoutLedgerEntryIds payoutRequest = do
  mbStashed <- Redis.get (makePayoutEntryIdsKey payoutRequest.id.getId)
  case fromMaybe [] (mbStashed <|> payoutRequest.ledgerEntryIds) of
    [] -> map (.id.getId) <$> LedgerService.findBySettlementId payoutRequest.id.getId
    entryIds -> pure entryIds

clearPayoutLedgerEntryIds :: (CacheFlow m r) => Text -> m ()
clearPayoutLedgerEntryIds = Redis.del . makePayoutEntryIdsKey

-- | Submit a payout request: creates the PayoutRequest (INITIATED),
--   then immediately executes via the external payout service (→ PROCESSING).
--   Used by rider cashback and the registration refund. The driver wallet payout creates the
--   request itself and calls 'executePayoutRequestWithOutcome', so it can write its hold in between.
--
--   Domain provides:
--     1. A 'PayoutSubmission' (flat data)
--     2. The payout call function (closure over service config)
submitPayoutRequest ::
  ( EncFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r,
    Finance.HasActorInfo m r
  ) =>
  PayoutSubmission ->
  (DPayment.CreatePayoutServiceReq -> m IPayout.CreatePayoutOrderResp) ->
  (PayoutOrder.PayoutOrder -> m ()) -> -- afterPayoutOrderCreated: runs once the order is persisted (e.g. schedule a status check job)
  m PayoutResult
submitPayoutRequest submission payoutCall afterPayoutOrderCreated = do
  -- 1. Build and persist PayoutRequest (INITIATED)
  payoutRequest <- buildPayoutRequest submission
  createPayoutRequest payoutRequest

  logDebug $ "Created PayoutRequest " <> payoutRequest.id.getId <> " for " <> submission.beneficiaryId <> " | amount: " <> show submission.amount

  -- NOTE (reviewer, remove before merge): same as main for Juspay/Stripe: Executed -> PayoutInitiated,
  --   ConfirmedFailure -> PayoutFailed (same "Payout service error: ..." text), NotExecutable ->
  --   PayoutProcessing, and no ledger write here. AmbiguousFailure -> PayoutAmbiguous is the only
  --   addition (BulkFlow only). Rider cashback still calls the partner first and holds after, as on main.
  -- 2. Execute. Ledger entries are not reserved here, as on main: the caller's
  -- OwnerPayoutLiability hold is what takes the amount out of the payable balance.
  outcome <- executePayoutRequestInternal submission.transferAmount submission.currency submission.payoutServiceFlow payoutRequest payoutCall afterPayoutOrderCreated
  pure $ case outcome of
    Executed po -> PayoutInitiated payoutRequest po
    ConfirmedFailure msg -> PayoutFailed payoutRequest msg
    AmbiguousFailure msg -> PayoutAmbiguous payoutRequest msg
    -- Already in flight or otherwise not executable. Main reports this as PayoutProcessing and
    -- its consumer (CashbackPayout) matches on it, so keep that mapping.
    NotExecutable status -> PayoutProcessing payoutRequest status

-- NOTE (reviewer, remove before merge): same behaviour as main: Just order on success, Nothing
--   otherwise; only the outcome type's names differ. Its only caller is the driver's
--   SpecialZonePayout job, which calls it as on main.

-- | Execute a previously created PayoutRequest by calling the external payout service.
--   Builds 'CreatePayoutOrderReq' from the stored fields in PayoutRequest.
--   The domain only provides the payout call function.
--
--   Used for scheduled payouts where the request was created earlier.
executePayoutRequest ::
  ( EncFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r,
    Finance.HasActorInfo m r
  ) =>
  Currency ->
  Payout.PayoutServiceFlow ->
  PayoutRequest ->
  (DPayment.CreatePayoutServiceReq -> m Payout.CreatePayoutOrderResp) ->
  (PayoutOrder.PayoutOrder -> m ()) ->
  m (Maybe PayoutOrder.PayoutOrder)
executePayoutRequest currency payoutServiceFlow payoutRequest payoutCall afterPayoutOrderCreated = do
  outcome <- executePayoutRequestInternal Nothing currency payoutServiceFlow payoutRequest payoutCall afterPayoutOrderCreated
  pure $ case outcome of
    Executed po -> Just po
    ConfirmedFailure _ -> Nothing
    AmbiguousFailure _ -> Nothing
    NotExecutable _ -> Nothing

-- NOTE (reviewer, remove before merge): this is executePayoutRequestInternal, exported. Its only
--   caller is the driver's WalletPayout.initiateWalletPayoutWith: create the request -> write the
--   hold -> this call -> give the money back if the payout was not sent. Juspay/Stripe difference
--   from main (driver wallet only): main called the partner first and wrote the hold after. Here a
--   refused payout leaves a hold plus a reversal ("Payout not sent: ..."), and a failed hold sends
--   nothing. Net money is the same as main.

-- | Create the order and call the partner for a request that already exists, and say what
--   happened, so the caller can give back its hold when the payout was not sent. Used by the driver
--   wallet payout, which writes its hold between creating the request and calling the partner.
executePayoutRequestWithOutcome ::
  ( EncFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r,
    Finance.HasActorInfo m r
  ) =>
  Maybe HighPrecMoney -> -- explicit transferAmount override (Nothing = use amount)
  Currency ->
  Payout.PayoutServiceFlow ->
  PayoutRequest ->
  (DPayment.CreatePayoutServiceReq -> m IPayout.CreatePayoutOrderResp) ->
  (PayoutOrder.PayoutOrder -> m ()) ->
  m ExecutionOutcome
executePayoutRequestWithOutcome = executePayoutRequestInternal

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- NOTE (reviewer, remove before merge): Executed / ConfirmedFailure / NotExecutable are main's
--   PayoutExecuted / PayoutExecutionFailed / PayoutNotExecutable under new names. AmbiguousFailure
--   is new and only BulkFlow returns it. isConfirmedRejection is only checked on the BulkFlow path,
--   so Juspay/Stripe never reach it.

-- | Outcome of an execution attempt, distinguishing a confirmed rejection (nothing was paid)
--   from a failure without a clear rejection (BulkFlow only; its order call is local, so nothing
--   reached the bank).
data ExecutionOutcome
  = Executed PayoutOrder.PayoutOrder
  | ConfirmedFailure Text
  | AmbiguousFailure Text
  | NotExecutable PayoutRequestStatus

-- | True only for FailureResponse -- the partner actually returned a decodable non-2xx we
--   understood as a rejection, so we know nothing was paid.
isConfirmedRejection :: SomeException -> Bool
isConfirmedRejection e = case fromException e of
  Just (extErr :: ExternalAPICallError) -> case extErr.clientError of
    FailureResponse _ _ -> True
    _ -> False
  Nothing -> False

-- | Internal: call Juspay via createPayoutService, manage status transitions.
executePayoutRequestInternal ::
  ( EncFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r,
    Finance.HasActorInfo m r
  ) =>
  Maybe HighPrecMoney -> -- explicit transferAmount override (Nothing = use amount)
  Currency ->
  Payout.PayoutServiceFlow ->
  PayoutRequest ->
  (DPayment.CreatePayoutServiceReq -> m IPayout.CreatePayoutOrderResp) ->
  (PayoutOrder.PayoutOrder -> m ()) ->
  m ExecutionOutcome
executePayoutRequestInternal mbTransferAmount currency payoutServiceFlow payoutRequest payoutCall afterPayoutOrderCreated = do
  if not (isPayoutExecutable payoutRequest)
    then do
      logInfo $ "PayoutRequest " <> payoutRequest.id.getId <> " not executable (status: " <> show payoutRequest.status <> "), skipping"
      pure $ NotExecutable payoutRequest.status
    else do
      orderId <- generateGUID
      createPayoutOrderReq <- buildCreatePayoutOrderReq orderId currency payoutRequest payoutServiceFlow mbTransferAmount
      let merchantId = Id payoutRequest.merchantId
          mbMocId = Just $ Id payoutRequest.merchantOperatingCityId
          personId = Id payoutRequest.beneficiaryId
          entityName = fromMaybe DCommon.DRIVER_WALLET_TRANSACTION payoutRequest.entityName
          city = fromMaybe "" payoutRequest.city

      logDebug $ "Executing payout for PayoutRequest " <> payoutRequest.id.getId <> " | orderId: " <> orderId <> " | amount: " <> show (fromMaybe 0 payoutRequest.amount)

      result <- try $ DPayment.createPayoutService merchantId mbMocId personId (Just [payoutRequest.id.getId]) (Just entityName) city createPayoutOrderReq payoutCall Nothing afterPayoutOrderCreated
      case result of
        -- NOTE (reviewer, remove before merge): the first guard (not BulkFlow) is main's single Left
        --   branch unchanged: AUTO_PAY_FAILED, "Payout service error: <err>", the same log line. So every
        --   Juspay/Stripe error, even one after the partner accepted (e.g. our own DB write failing), is a
        --   ConfirmedFailure, and the driver wallet payout gives its hold back; main never held here, so
        --   net money matches. The other two guards are bulk-only: the "partner call" is
        --   Bulk.localOrderCall (no HTTP), so any error is ours, and the driver wallet payout gives the
        --   money back for both results.
        Left (err :: SomeException)
          -- Only the bulk rail splits a confirmed rejection from an ambiguous one. Every other
          -- rail keeps main's behaviour exactly: AUTO_PAY_FAILED and a plain failure. No rail
          -- writes the ledger here -- the caller's OwnerPayoutLiability hold is settled or
          -- reversed by the settlement flow, and marking PROCESSING instead of failed would
          -- strand a Juspay payout.
          | payoutServiceFlow /= Payout.BulkFlow -> do
            let msg = "Payout service error: " <> show err
            logError $ "Payout service call failed for PayoutRequest " <> payoutRequest.id.getId <> ": " <> show err
            updateStatusWithHistoryById AUTO_PAY_FAILED (Just msg) payoutRequest
            pure $ ConfirmedFailure msg
          | isConfirmedRejection err -> do
            -- The partner returned a decodable non-2xx rejection.
            let msg = "Payout service error: " <> show err
            logError $ "Payout service call rejected for PayoutRequest " <> payoutRequest.id.getId <> ": " <> show err
            updateStatusWithHistoryById AUTO_PAY_FAILED (Just msg) payoutRequest
            pure $ ConfirmedFailure msg
          | otherwise -> do
            let msg = "Payout service call outcome unknown: " <> show err
            logError $ "Payout service call for PayoutRequest " <> payoutRequest.id.getId <> " errored ambiguously (may have reached the bank): " <> show err <> " -- leaving request PROCESSING, needs manual reconciliation"
            updateStatusWithHistoryById PROCESSING (Just msg) payoutRequest
            pure $ AmbiguousFailure msg
        Right (_mbResp, mbPayoutOrder) -> do
          let payoutOrderIdText = maybe "unknown" (\po -> po.id.getId) mbPayoutOrder
          QPR.updatePayoutTransactionIdById (Just payoutOrderIdText) payoutRequest.id
          updateStatusWithHistoryById PROCESSING (Just $ "Payout request sent to Bank. OrderId: " <> payoutOrderIdText) payoutRequest
          pure $ maybe (NotExecutable PROCESSING) Executed mbPayoutOrder

-- | Build a CreatePayoutOrderReq from the stored PayoutRequest fields.
--   Throws if VPA is missing — VPA must be populated at PayoutRequest creation time.
buildCreatePayoutOrderReq :: (MonadFlow m) => Text -> Currency -> PayoutRequest -> Payout.PayoutServiceFlow -> Maybe HighPrecMoney -> m DPayment.CreatePayoutServiceReq
buildCreatePayoutOrderReq orderId currency pr payoutServiceFlow mbTransferAmount = do
  vpa <- case payoutServiceFlow of
    Payout.JuspayFlow -> Just <$> fromMaybeM (InvalidRequest $ "VPA is required for payout but missing in PayoutRequest " <> pr.id.getId) pr.customerVpa
    Payout.StripeFlow -> pure $ pr.customerVpa
    -- NOTE (reviewer, remove before merge): the BulkFlow case (shared-kernel's PayoutServiceFlow has
    --   BulkFlow) and the pr.batchId argument below are bulk-only. The Juspay and Stripe cases are
    --   main's, and pr.batchId is NULL for them.
    -- Bulk partners (HDFC CBX) pay to an account number + IFSC, not a VPA. Bulk does come through
    -- here (the bulk claim and the bulk instant payout, via executePayoutRequestWithOutcome); the
    -- app's createPayoutOrder then writes a local order for the batch (Bulk.localOrderCall) instead
    -- of calling the partner.
    Payout.BulkFlow -> pure Nothing
  pure $
    DPayment.mkCreatePayoutServiceReq
      orderId
      (fromMaybe 0 pr.amount)
      currency
      pr.customerPhone
      pr.customerEmail
      pr.beneficiaryId
      (fromMaybe "Payout" pr.remark)
      pr.customerName
      vpa
      (fromMaybe "FULFILL_ONLY" pr.orderType)
      payoutServiceFlow
      mbTransferAmount
      pr.batchId

-- NOTE (reviewer, remove before merge): main's buildPayoutRequest plus copying batchId (Nothing
--   for Juspay/Stripe). It is exported (internal on main) so the driver wallet payout can create the
--   request, write the hold, then call executePayoutRequestWithOutcome.

-- | Build a PayoutRequest from a PayoutSubmission.
buildPayoutRequest ::
  (MonadFlow m) =>
  PayoutSubmission ->
  m PayoutRequest
buildPayoutRequest submission = do
  now <- getCurrentTime
  prId <- Id <$> generateGUID
  pure
    PayoutRequest
      { id = prId,
        entityName = Just submission.entityName,
        entityId = submission.entityId,
        entityRefId = submission.entityRefId,
        beneficiaryId = submission.beneficiaryId,
        batchId = submission.batchId,
        amount = Just submission.amount,
        status = INITIATED,
        retryCount = Nothing,
        failureReason = Nothing,
        payoutTransactionId = Nothing,
        cashMarkedById = Nothing,
        cashMarkedByName = Nothing,
        cashMarkedAt = Nothing,
        expectedCreditTime = Nothing,
        scheduledAt = submission.scheduledAt,
        customerVpa = submission.vpa,
        bankName = submission.bankName,
        bankAccountLast4 = submission.bankAccountLast4,
        customerPhone = submission.customerPhone,
        customerEmail = submission.customerEmail,
        customerName = submission.customerName,
        remark = Just submission.remark,
        orderType = Just submission.orderType,
        city = Just submission.city,
        merchantId = submission.merchantId,
        merchantOperatingCityId = submission.merchantOpCityId,
        payoutFee = submission.payoutFee,
        payoutType = submission.payoutType,
        coverageFrom = submission.coverageFrom,
        coverageTo = submission.coverageTo,
        ledgerEntryIds = if null submission.ledgerEntryIds then Nothing else Just submission.ledgerEntryIds,
        createdAt = now,
        updatedAt = now
      }
