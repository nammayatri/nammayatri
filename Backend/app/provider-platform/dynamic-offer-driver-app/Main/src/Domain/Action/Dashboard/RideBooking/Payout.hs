{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.RideBooking.Payout
  ( getPayoutStatus,
    postPayoutCancel,
    postPayoutRetry,
    postPayoutMarkCashPaid,
  )
where

-- we could possibly implement the whole logic here too.
import qualified API.Types.Dashboard.RideBooking.Payout as API
import Data.Maybe (listToMaybe)
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Merchant
import qualified Domain.Types.PayoutStatusHistory as DPSH
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ScheduledPayout as DSP
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payout.Interface.Types as IPayout
import qualified Kernel.External.Payout.Types as TPayout
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPO
import qualified SharedLogic.Allocator.Jobs.Payout.SpecialZonePayout as SpecialZonePayout
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.PayoutStatusHistory as QPSH
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ScheduledPayout as QSP
import qualified Storage.Queries.ScheduledPayoutExtra as QSP
import Tools.Error
import qualified Tools.Payout as Payout

getPayoutStatus ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Environment.Flow API.PayoutStatusResp
getPayoutStatus _merchantShortId _opCity rideId = do
  payout <- QSP.findByRideId rideId >>= fromMaybeM (InvalidRequest "Payout not found for this ride")
  merchantId <- payout.merchantId & fromMaybeM (InvalidRequest "Merchant not found")
  merchantOpCityId <- payout.merchantOperatingCityId & fromMaybeM (InvalidRequest "Merchant Operating City not found")
  payoutOrders <- QPO.findAllByEntityNameAndEntityIds (Just 1) (Just 0) (Just DPayment.SPECIAL_ZONE_PAYOUT) [Just payout.id.getId]
  updatedStatus <- do
    case (listToMaybe payoutOrders) of
      Just payoutOrder -> do
        let createPayoutOrderStatusReq = IPayout.PayoutOrderStatusReq {orderId = payoutOrder.orderId, mbExpand = Nothing}
            createPayoutOrderStatusCall = Payout.payoutOrderStatus merchantId merchantOpCityId (DEMSC.RidePayoutService TPayout.Juspay) (Just payout.driverId)
        resp <- DPayment.payoutStatusService (Kernel.Types.Id.cast merchantId) (Kernel.Types.Id.Id payout.driverId) createPayoutOrderStatusReq createPayoutOrderStatusCall
        let newStatus = QSP.castPayoutOrderStatusToScheduledPayoutStatus resp.status
        if (payout.status /= newStatus && payout.status `notElem` [DSP.CREDITED, DSP.CASH_PAID, DSP.CASH_PENDING])
          then do
            let statusMsg = "Order Status Updated: " <> show resp.status
            QSP.updateStatusWithHistoryById newStatus (Just statusMsg) payout
            pure newStatus
          else pure payout.status
      Nothing -> do
        logError $ "Payout Order not found for scheduled payoutId: " <> show payout.id
        pure payout.status

  statusHistory <- QPSH.findByScheduledPayoutId Nothing Nothing payout.id
  pure $
    API.PayoutStatusResp
      { status = convertStatus updatedStatus,
        amount = payout.amount,
        rideId = payout.rideId,
        payoutTransactionId = payout.payoutTransactionId,
        expectedCreditTime = payout.expectedCreditTime,
        failureReason = payout.failureReason,
        statusHistory = map convertHistory statusHistory,
        createdAt = payout.createdAt,
        updatedAt = payout.updatedAt
      }

-- | Cancel payout before 2 hours (admin only)
postPayoutCancel ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  API.PayoutCancelReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postPayoutCancel _merchantShortId _opCity rideId req = do
  payout <- QSP.findByRideId rideId >>= fromMaybeM (InvalidRequest "Payout not found for this ride")
  now <- getCurrentTime

  -- Validation: Can only cancel if INITIATED
  unless (payout.status == DSP.INITIATED) $
    throwError $ InvalidRequest "Can only cancel INITIATED payouts"

  -- Validation: Can only cancel within 2 hours of creation
  let cutoffTime = addUTCTime (2 * 60 * 60) payout.createdAt
  unless (now < cutoffTime) $
    throwError $ InvalidRequest "Cancel window expired (2 hours)"

  -- Update status to CANCELLED with reason and record history
  QSP.updateStatusWithReasonByRideId DSP.CANCELLED (Just req.reason) rideId
  QSP.updateStatusWithHistoryById DSP.CANCELLED (Just $ "Cancelled: " <> req.reason) payout

  logInfo $ "Payout cancelled for rideId: " <> rideId <> " reason: " <> req.reason
  pure Kernel.Types.APISuccess.Success

-- | Retry failed payout immediately (admin only)
postPayoutRetry ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postPayoutRetry _merchantShortId _opCity rideId = do
  payout <- QSP.findByRideId rideId >>= fromMaybeM (InvalidRequest "Payout not found for this ride")

  -- Validation: Can only retry AUTO_PAY_FAILED payouts
  unless (payout.status == DSP.AUTO_PAY_FAILED) $
    throwError $ InvalidRequest "Can only retry AUTO_PAY_FAILED payouts"

  -- Validation: Can only retry once (retryCount must be Nothing or 0)
  let currentRetryCount = fromMaybe 0 payout.retryCount
  unless (currentRetryCount == 0) $
    throwError $ InvalidRequest "Retry limit exceeded (max 1 retry)"

  -- Increment retry count
  QSP.incrementRetryCountByRideId (Just 1) rideId

  logInfo $ "Retrying payout for rideId: " <> rideId

  -- Mark as RETRYING with history
  QSP.updateStatusWithHistoryById DSP.RETRYING (Just "Admin initiated retry...") payout

  -- Execute the same payout logic as in SpecialZonePayout
  _ <- SpecialZonePayout.executeSpecialZonePayout payout

  logInfo $ "Payout retry completed for rideId: " <> rideId
  pure Kernel.Types.APISuccess.Success

-- Helper to convert domain status to API status
convertStatus :: DSP.ScheduledPayoutStatus -> API.PayoutStatus
convertStatus DSP.INITIATED = API.INITIATED
convertStatus DSP.PROCESSING = API.PROCESSING
convertStatus DSP.CREDITED = API.CREDITED
convertStatus DSP.AUTO_PAY_FAILED = API.AUTO_PAY_FAILED
convertStatus DSP.RETRYING = API.RETRYING
convertStatus DSP.FAILED = API.FAILED
convertStatus DSP.CANCELLED = API.CANCELLED
convertStatus DSP.CASH_PAID = API.CASH_PAID
convertStatus DSP.CASH_PENDING = API.CASH_PENDING

-- Helper to convert domain status history to API status event
convertHistory :: DPSH.PayoutStatusHistory -> API.PayoutStatusEvent
convertHistory h =
  API.PayoutStatusEvent
    { status = convertStatus h.status,
      timestamp = h.createdAt,
      message = h.message
    }

postPayoutMarkCashPaid ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postPayoutMarkCashPaid _merchantShortId _opCity rideId mbAgentIdText = do
  agentIdText <- mbAgentIdText & fromMaybeM (InvalidRequest "Agent Id is required")
  payout <- QSP.findByRideId rideId >>= fromMaybeM (InvalidRequest "Payout not found for this ride")
  agent <- QPerson.findById (Kernel.Types.Id.Id agentIdText) >>= fromMaybeM (InvalidRequest $ "Agent not found for id: " <> agentIdText)
  when (payout.status == DSP.CREDITED) $ do
    throwError $ InvalidRequest "Payout is already credited online via bank!"
  when (payout.status == DSP.PROCESSING) $ do
    throwError $ InvalidRequest "Payout is already being processed for online credit!"
  when (payout.status == DSP.CASH_PAID) $ do
    throwError $ InvalidRequest "Payout is already marked as cash paid!"
  let agentName = agent.firstName <> " " <> (fromMaybe "" agent.lastName)
  QSP.updateStatusWithHistoryById DSP.CASH_PAID (Just $ "Cash Paid Marked by " <> agentName) payout
  QSP.updateMarkCashPaidByById (Just agent.id) payout.id
  logInfo $ "Cash paid marked for rideId: " <> rideId
  pure Kernel.Types.APISuccess.Success
