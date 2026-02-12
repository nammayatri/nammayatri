{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.API.Payout
  ( DashboardAPI,
    UIAPI,
    VerifyVpaFlow (..),
    PayoutUIHandlerConfig (..),
    PayoutDashboardHandlerConfig (..),
    payoutUIHandler,
    payoutDashboardHandler,
  )
where

import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error (GenericError (InvalidRequest))
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (fromMaybeM, throwError)
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import Lib.Payment.API.Payout.Types
import Lib.Payment.Domain.Types.Common (EntityName)
import Lib.Payment.Domain.Types.PayoutRequest (PayoutRequest (..), PayoutRequestStatus (..))
import qualified Lib.Payment.Payout.History as PayoutHistory
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import Servant hiding (throwError)

type UIAPI =
  "payout"
    :> "by-entity"
    :> QueryParam' '[Required] "entityName" EntityName
    :> QueryParam' '[Required] "entityId" Text
    :> Get '[JSON] PayoutRequestResp

type DashboardAPI =
  "payout"
    :> ( Capture "payoutRequestId" (Id.Id PayoutRequest)
           :> Get '[JSON] PayoutRequestResp
           :<|> Capture "payoutRequestId" (Id.Id PayoutRequest)
           :> "retry"
           :> Post '[JSON] PayoutSuccess
           :<|> Capture "payoutRequestId" (Id.Id PayoutRequest)
           :> "cancel"
           :> ReqBody '[JSON] PayoutCancelReq
           :> Post '[JSON] PayoutSuccess
           :<|> Capture "payoutRequestId" (Id.Id PayoutRequest)
           :> "cash"
           :> ReqBody '[JSON] PayoutCashUpdateReq
           :> Post '[JSON] PayoutSuccess
           :<|> "vpa"
           :> "delete"
           :> ReqBody '[JSON] DeleteVpaReq
           :> Post '[JSON] PayoutSuccess
           :<|> "vpa"
           :> "update"
           :> ReqBody '[JSON] UpdateVpaReq
           :> Post '[JSON] PayoutSuccess
           :<|> "vpa"
           :> "refundRegistration"
           :> ReqBody '[JSON] RefundRegAmountReq
           :> Post '[JSON] PayoutSuccess
       )

data PayoutUIHandlerConfig m = PayoutUIHandlerConfig
  { refreshPayoutRequest :: PayoutRequest -> m PayoutRequest
  }

data PayoutDashboardHandlerConfig m = PayoutDashboardHandlerConfig
  { refreshPayoutRequest :: PayoutRequest -> m PayoutRequest,
    executePayoutRetry :: PayoutRequest -> m (),
    handleDeleteVpa :: DeleteVpaReq -> m PayoutSuccess,
    handleUpdateVpa :: UpdateVpaReq -> m PayoutSuccess,
    handleRefundRegistrationAmount :: RefundRegAmountReq -> m PayoutSuccess
  }

class VerifyVpaFlow m where
  verifyVpaForUpdate :: UpdateVpaReq -> m ()

payoutUIHandler ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r) =>
  PayoutUIHandlerConfig m ->
  ServerT UIAPI m
payoutUIHandler cfg =
  getPayoutByEntity
  where
    getPayoutByEntity entityName entityId = do
      payoutRequest <- QPR.findByEntity entityId (Just entityName) >>= fromMaybeM (InvalidRequest "Payout request not found")
      refreshed <- cfg.refreshPayoutRequest payoutRequest
      buildPayoutResp refreshed

payoutDashboardHandler ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r, VerifyVpaFlow m) =>
  PayoutDashboardHandlerConfig m ->
  ServerT DashboardAPI m
payoutDashboardHandler cfg =
  getPayoutById
    :<|> retryPayout
    :<|> cancelPayout
    :<|> markCash
    :<|> cfg.handleDeleteVpa
    :<|> updateVpa
    :<|> cfg.handleRefundRegistrationAmount
  where
    getPayoutById payoutRequestId = do
      payoutRequest <- QPR.findById payoutRequestId >>= fromMaybeM (InvalidRequest "Payout request not found")
      refreshed <- cfg.refreshPayoutRequest payoutRequest
      buildPayoutResp refreshed

    retryPayout payoutRequestId = do
      payoutRequest <- QPR.findById payoutRequestId >>= fromMaybeM (InvalidRequest "Payout request not found")
      PayoutRequest.retryPayoutWith canRetry cfg.executePayoutRetry payoutRequest
      pure Success

    cancelPayout payoutRequestId req = do
      payoutRequest <- QPR.findById payoutRequestId >>= fromMaybeM (InvalidRequest "Payout request not found")
      PayoutRequest.cancelPayoutWithin (2 * 60 * 60) req.reason payoutRequest
      pure Success

    markCash payoutRequestId req = do
      payoutRequest <- QPR.findById payoutRequestId >>= fromMaybeM (InvalidRequest "Payout request not found")
      case req.status of
        CASH_PENDING -> PayoutRequest.markCashPending req.agentId req.agentName req.message payoutRequest
        CASH_PAID -> PayoutRequest.markCashPaid req.agentId req.agentName req.message payoutRequest
        _ -> throwError $ InvalidRequest "Invalid cash status"
      pure Success

    updateVpa req = do
      when req.verify $ verifyVpaForUpdate req
      cfg.handleUpdateVpa req

    canRetry status = status `elem` [AUTO_PAY_FAILED, FAILED, PROCESSING, INITIATED]

buildPayoutResp ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r) =>
  PayoutRequest ->
  m PayoutRequestResp
buildPayoutResp payoutRequest = do
  history <- PayoutHistory.getPayoutHistory "PayoutRequest" payoutRequest.id.getId
  let statusHistory = mapMaybe toStatusEvent history
  pure $
    PayoutRequestResp
      { payoutRequestId = payoutRequest.id,
        entityName = payoutRequest.entityName,
        entityId = payoutRequest.entityId,
        entityRefId = payoutRequest.entityRefId,
        beneficiaryId = payoutRequest.beneficiaryId,
        amount = payoutRequest.amount,
        status = payoutRequest.status,
        retryCount = payoutRequest.retryCount,
        failureReason = payoutRequest.failureReason,
        payoutTransactionId = payoutRequest.payoutTransactionId,
        expectedCreditTime = payoutRequest.expectedCreditTime,
        scheduledAt = payoutRequest.scheduledAt,
        cashMarkedById = payoutRequest.cashMarkedById,
        cashMarkedByName = payoutRequest.cashMarkedByName,
        cashMarkedAt = payoutRequest.cashMarkedAt,
        statusHistory = statusHistory,
        createdAt = payoutRequest.createdAt,
        updatedAt = payoutRequest.updatedAt
      }
  where
    toStatusEvent h = do
      status <- parseStatus h.status
      pure $
        PayoutStatusEvent
          { status = status,
            timestamp = h.timestamp,
            message = h.message
          }

parseStatus :: Text -> Maybe PayoutRequestStatus
parseStatus statusText = readMaybe (T.unpack statusText)
