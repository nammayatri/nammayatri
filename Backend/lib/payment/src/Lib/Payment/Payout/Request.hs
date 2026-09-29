{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.Payout.Request
  ( PayoutRequest (..),
    PayoutRequestStatus (..),
    PayoutSubmission (..),
    PayoutResult (..),
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
    -- | Set only by the HDFC bulk flow; every other payout has no batch.
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

-- | Result of a payout submission or execution.
--   PayoutFailed means nothing was paid. PayoutAmbiguous (bulk only) means no clear rejection, so
--   the request stays PROCESSING.
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
--   Used by rider cashback and the registration refund. The driver wallet payout uses
--   'executePayoutRequestWithOutcome' so it can write its hold in between.
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

  -- 2. Execute. No ledger reservation here: the caller's OwnerPayoutLiability hold does that.
  outcome <- executePayoutRequestInternal submission.transferAmount submission.currency submission.payoutServiceFlow payoutRequest payoutCall afterPayoutOrderCreated
  pure $ case outcome of
    Executed po -> PayoutInitiated payoutRequest po
    ConfirmedFailure msg -> PayoutFailed payoutRequest msg
    AmbiguousFailure msg -> PayoutAmbiguous payoutRequest msg
    -- Already in flight or not executable. CashbackPayout matches on PayoutProcessing.
    NotExecutable status -> PayoutProcessing payoutRequest status

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

-- | Execute an existing request and report the outcome, so the caller can give back its hold when
--   nothing was sent. Used by the driver wallet payout.
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

-- | Outcome of one execution attempt. ConfirmedFailure means nothing was paid; AmbiguousFailure
--   (bulk only) means no clear rejection.
data ExecutionOutcome
  = Executed PayoutOrder.PayoutOrder
  | ConfirmedFailure Text
  | AmbiguousFailure Text
  | NotExecutable PayoutRequestStatus

-- | True only when the partner returned a non-2xx we could decode, so we know nothing was paid.
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
        Left (err :: SomeException)
          -- Only bulk splits a confirmed rejection from an ambiguous one. Other rails fail the
          -- request, since leaving it PROCESSING would strand a Juspay payout. No ledger write
          -- here: settlement handles the caller's hold.
          | payoutServiceFlow /= Payout.BulkFlow -> do
            let msg = "Payout service error: " <> show err
            logError $ "Payout service call failed for PayoutRequest " <> payoutRequest.id.getId <> ": " <> show err
            updateStatusWithHistoryById AUTO_PAY_FAILED (Just msg) payoutRequest
            pure $ ConfirmedFailure msg
          | isConfirmedRejection err -> do
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
    -- HDFC CBX pays to an account number and IFSC, not a VPA. Its order call is local
    -- (Bulk.localOrderCall), so no partner is called here.
    Payout.BulkFlow -> pure Nothing
  -- Juspay rejects amounts with more than 2 decimal places (E01)
  let amount = fromMaybe 0 pr.amount
      sentAmount = DPayment.floorToTwoDecimalPlaces amount
  when (sentAmount /= amount) $
    logInfo $ "PayoutRequest " <> pr.id.getId <> " amount floored to 2 decimals: ledger=" <> show amount <> " sent=" <> show sentAmount <> " diff=" <> show (amount - sentAmount)
  pure $
    DPayment.mkCreatePayoutServiceReq
      orderId
      sentAmount
      currency
      pr.customerPhone
      pr.customerEmail
      pr.beneficiaryId
      (fromMaybe "Payout" pr.remark)
      pr.customerName
      vpa
      (fromMaybe "FULFILL_ONLY" pr.orderType)
      payoutServiceFlow
      (DPayment.floorToTwoDecimalPlaces <$> mbTransferAmount)
      pr.batchId

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
