{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.RideBooking.Payout
  ( getPayoutStatus,
    postPayoutCancel,
    postPayoutRetry,
  )
where

-- we could possibly implement the whole logic here too.
import qualified API.Types.Dashboard.RideBooking.Payout as API
import qualified Domain.Types.Merchant
import qualified Domain.Types.PayoutStatusHistory as DPSH
import qualified Domain.Types.ScheduledPayout as DSP
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Allocator.Jobs.Payout.SpecialZonePayout as SpecialZonePayout
import qualified Storage.Queries.PayoutStatusHistory as QPSH
import qualified Storage.Queries.ScheduledPayout as QSP
import qualified Storage.Queries.ScheduledPayoutExtra as QSPE
import Tools.Error

getPayoutStatus ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Environment.Flow API.PayoutStatusResp
getPayoutStatus _merchantShortId _opCity rideId = do
  payout <- QSP.findByRideId rideId >>= fromMaybeM (InvalidRequest "Payout not found for this ride")
  statusHistory <- QPSH.findByScheduledPayoutId Nothing Nothing payout.id
  pure $
    API.PayoutStatusResp
      { status = convertStatus payout.status,
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

  -- Update status to FAILED with reason and record history
  QSP.updateStatusWithReasonByRideId DSP.FAILED (Just req.reason) rideId
  QSPE.updateStatusWithHistoryById DSP.FAILED (Just $ "Cancelled: " <> req.reason) payout

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
  QSPE.updateStatusWithHistoryById DSP.RETRYING (Just "Admin initiated retry...") payout

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

-- Helper to convert domain status history to API status event
convertHistory :: DPSH.PayoutStatusHistory -> API.PayoutStatusEvent
convertHistory h =
  API.PayoutStatusEvent
    { status = convertStatus h.status,
      timestamp = h.createdAt,
      message = h.message
    }
