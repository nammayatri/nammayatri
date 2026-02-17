{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.Payout.Request
  ( PayoutRequest (..),
    PayoutRequestStatus (..),
    PayoutSubmission (..),
    PayoutResult (..),
    createPayoutRequest,
    submitPayoutRequest,
    executePayoutRequest,
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
  )
where

import Control.Applicative ((<|>))
import Data.Time.Clock (addUTCTime)
import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.Payout.Interface.Types as IPayout
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (HighPrecMoney, MonadFlow, fromMaybeM, generateGUID, getCurrentTime, logDebug, logError, logInfo, throwError)
import qualified Lib.Finance.Domain.Types.StateTransition as ST
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DCommon
import qualified Lib.Payment.Domain.Types.PayoutOrder as PayoutOrder
import Lib.Payment.Domain.Types.PayoutRequest
import qualified Lib.Payment.Payout.History as PayoutHistory
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR

-- | Flat data record supplied by the domain to request a payout.
--   The lib uses this to construct both the PayoutRequest and CreatePayoutOrderReq.
data PayoutSubmission = PayoutSubmission
  { beneficiaryId :: Text,
    entityName :: DCommon.EntityName,
    entityId :: Text,
    entityRefId :: Maybe Text,
    amount :: HighPrecMoney,
    payoutFee :: Maybe HighPrecMoney,
    merchantId :: Text,
    merchantOpCityId :: Text,
    city :: Text,
    vpa :: Text,
    customerName :: Maybe Text,
    customerPhone :: Maybe Text,
    customerEmail :: Maybe Text,
    remark :: Text,
    orderType :: Text,
    scheduledAt :: Maybe UTCTime
  }
  deriving (Show, Generic)

-- | Result of a payout submission or execution.
data PayoutResult
  = PayoutInitiated PayoutRequest PayoutOrder.PayoutOrder
  | PayoutFailed PayoutRequest Text

-- ---------------------------------------------------------------------------
-- CRUD
-- ---------------------------------------------------------------------------

createPayoutRequest ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r) =>
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
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r) =>
  PayoutRequestStatus ->
  Maybe Text ->
  PayoutRequest ->
  m ()
updateStatusWithHistoryById newStatus message payoutRequest = do
  QPR.updateStatusById newStatus payoutRequest.id
  recordHistory (Just payoutRequest.status) newStatus message payoutRequest

createInitialHistory ::
  (FinanceBeamFlow.BeamFlow m r) =>
  PayoutRequest ->
  m ()
createInitialHistory payoutRequest = do
  recordHistory Nothing payoutRequest.status (Just $ getStatusMessage payoutRequest.status) payoutRequest

markCashPending ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r) =>
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
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r) =>
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
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r) =>
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
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r) =>
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

-- | Submit a payout request: creates the PayoutRequest (INITIATED),
--   then immediately executes via the external payout service (→ PROCESSING).
--   This is the single entry point for instant payouts.
--
--   Domain provides:
--     1. A 'PayoutSubmission' (flat data)
--     2. The payout call function (closure over service config)
submitPayoutRequest ::
  ( EncFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r
  ) =>
  PayoutSubmission ->
  (IPayout.CreatePayoutOrderReq -> m IPayout.CreatePayoutOrderResp) ->
  m PayoutResult
submitPayoutRequest submission payoutCall = do
  -- 1. Build and persist PayoutRequest (INITIATED)
  payoutRequest <- buildPayoutRequest submission
  createPayoutRequest payoutRequest

  logDebug $ "Created PayoutRequest " <> payoutRequest.id.getId <> " for " <> submission.beneficiaryId <> " | amount: " <> show submission.amount

  -- 2. Execute
  mbPayoutOrder <- executePayoutRequestInternal payoutRequest payoutCall
  case mbPayoutOrder of
    Just po -> pure $ PayoutInitiated payoutRequest po
    Nothing -> pure $ PayoutFailed payoutRequest "Payout service call failed"

-- | Execute a previously created PayoutRequest by calling the external payout service.
--   Builds 'CreatePayoutOrderReq' from the stored fields in PayoutRequest.
--   The domain only provides the payout call function.
--
--   Used for scheduled payouts where the request was created earlier.
executePayoutRequest ::
  ( EncFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r
  ) =>
  PayoutRequest ->
  (IPayout.CreatePayoutOrderReq -> m IPayout.CreatePayoutOrderResp) ->
  m (Maybe PayoutOrder.PayoutOrder)
executePayoutRequest = executePayoutRequestInternal

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Internal: call Juspay via createPayoutService, manage status transitions.
executePayoutRequestInternal ::
  ( EncFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r
  ) =>
  PayoutRequest ->
  (IPayout.CreatePayoutOrderReq -> m IPayout.CreatePayoutOrderResp) ->
  m (Maybe PayoutOrder.PayoutOrder)
executePayoutRequestInternal payoutRequest payoutCall = do
  if not (isPayoutExecutable payoutRequest)
    then do
      logInfo $ "PayoutRequest " <> payoutRequest.id.getId <> " not executable (status: " <> show payoutRequest.status <> "), skipping"
      pure Nothing
    else do
      orderId <- generateGUID
      createPayoutOrderReq <- buildCreatePayoutOrderReq orderId payoutRequest
      let merchantId = Id payoutRequest.merchantId
          mbMocId = Just $ Id payoutRequest.merchantOperatingCityId
          personId = Id payoutRequest.beneficiaryId
          entityName = fromMaybe DCommon.DRIVER_WALLET_TRANSACTION payoutRequest.entityName
          city = fromMaybe "" payoutRequest.city

      logDebug $ "Executing payout for PayoutRequest " <> payoutRequest.id.getId <> " | orderId: " <> orderId <> " | amount: " <> show (fromMaybe 0 payoutRequest.amount)

      result <- try $ DPayment.createPayoutService merchantId mbMocId personId (Just [payoutRequest.id.getId]) (Just entityName) city createPayoutOrderReq payoutCall
      case result of
        Left (err :: SomeException) -> do
          logError $ "Payout service call failed for PayoutRequest " <> payoutRequest.id.getId <> ": " <> show err
          updateStatusWithHistoryById AUTO_PAY_FAILED (Just $ "Payout service error: " <> show err) payoutRequest
          pure Nothing
        Right (_mbResp, mbPayoutOrder) -> do
          let payoutOrderIdText = maybe "unknown" (\po -> po.id.getId) mbPayoutOrder
          QPR.updatePayoutTransactionIdById (Just payoutOrderIdText) payoutRequest.id
          updateStatusWithHistoryById PROCESSING (Just $ "Payout request sent to Bank. OrderId: " <> payoutOrderIdText) payoutRequest
          pure mbPayoutOrder

-- | Build a CreatePayoutOrderReq from the stored PayoutRequest fields.
--   Throws if VPA is missing — VPA must be populated at PayoutRequest creation time.
buildCreatePayoutOrderReq :: (MonadFlow m) => Text -> PayoutRequest -> m IPayout.CreatePayoutOrderReq
buildCreatePayoutOrderReq orderId pr = do
  vpa <- fromMaybeM (InvalidRequest $ "VPA is required for payout but missing in PayoutRequest " <> pr.id.getId) pr.customerVpa
  pure $
    DPayment.mkCreatePayoutOrderReq
      orderId
      (fromMaybe 0 pr.amount)
      pr.customerPhone
      pr.customerEmail
      pr.beneficiaryId
      (fromMaybe "Payout" pr.remark)
      pr.customerName
      vpa
      (fromMaybe "FULFILL_ONLY" pr.orderType)
      False

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
        customerVpa = Just submission.vpa,
        customerPhone = submission.customerPhone,
        customerEmail = submission.customerEmail,
        customerName = submission.customerName,
        remark = Just submission.remark,
        orderType = Just submission.orderType,
        city = Just submission.city,
        merchantId = submission.merchantId,
        merchantOperatingCityId = submission.merchantOpCityId,
        payoutFee = submission.payoutFee,
        createdAt = now,
        updatedAt = now
      }

-- ---------------------------------------------------------------------------
-- History & state helpers
-- ---------------------------------------------------------------------------

recordHistory ::
  (FinanceBeamFlow.BeamFlow m r) =>
  Maybe PayoutRequestStatus ->
  PayoutRequestStatus ->
  Maybe Text ->
  PayoutRequest ->
  m ()
recordHistory fromStatus toStatus message payoutRequest = do
  let merchantIdText = payoutRequest.merchantId
      opCityIdText = payoutRequest.merchantOperatingCityId
  void $
    PayoutHistory.recordPayoutHistory
      PayoutHistory.PayoutHistoryRecord
        { entityType = "PayoutRequest",
          entityId = payoutRequest.id.getId,
          fromState = toPaymentState <$> fromStatus,
          toState = toPaymentState toStatus,
          message = message,
          metadata = Nothing,
          actorType = "SYSTEM",
          actorId = Nothing,
          merchantId = merchantIdText,
          merchantOperatingCityId = opCityIdText
        }

toPaymentState :: PayoutRequestStatus -> ST.PaymentState
toPaymentState INITIATED = ST.INITIATED
toPaymentState PROCESSING = ST.PROCESSING
toPaymentState CREDITED = ST.CREDITED
toPaymentState AUTO_PAY_FAILED = ST.AUTO_PAY_FAILED
toPaymentState RETRYING = ST.RETRYING
toPaymentState FAILED = ST.FAILED
toPaymentState CANCELLED = ST.CANCELLED
toPaymentState CASH_PAID = ST.CASH_PAID
toPaymentState CASH_PENDING = ST.CASH_PENDING

getStatusMessage :: PayoutRequestStatus -> Text
getStatusMessage INITIATED = "Payout scheduled"
getStatusMessage PROCESSING = "Payment in progress"
getStatusMessage CREDITED = "Payment credited to bank"
getStatusMessage AUTO_PAY_FAILED = "Auto-pay failed"
getStatusMessage CANCELLED = "Payment cancelled"
getStatusMessage CASH_PAID = "Payment marked as cash paid"
getStatusMessage CASH_PENDING = "Payment marked as cash pending"
getStatusMessage RETRYING = "Retrying payment..."
getStatusMessage FAILED = "Payment failed/cancelled"
