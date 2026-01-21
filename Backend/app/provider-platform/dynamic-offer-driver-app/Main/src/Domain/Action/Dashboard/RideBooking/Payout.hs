{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.RideBooking.Payout
  ( getPayoutStatus,
    postPayoutCancel,
    postPayoutRetry,
  )
where

import qualified API.Types.Dashboard.RideBooking.Payout as API
import qualified Domain.Types.Merchant
import qualified Domain.Types.ScheduledPayout as DSP
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.ScheduledPayout as QSP
import Tools.Error

-- | Get payout status by rideId (no auth - for agent)
getPayoutStatus ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Environment.Flow API.PayoutStatusResp
getPayoutStatus _merchantShortId _opCity rideId = do
  payout <- QSP.findByRideId rideId >>= fromMaybeM (InvalidRequest "Payout not found for this ride")
  pure $
    API.PayoutStatusResp
      { status = convertStatus payout.status,
        amount = payout.amount,
        rideId = payout.rideId,
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

  -- Validation: Can only cancel if PENDING
  unless (payout.status == DSP.PENDING) $
    throwError $ InvalidRequest "Can only cancel PENDING payouts"

  -- Validation: Can only cancel within 2 hours of creation
  let cutoffTime = addUTCTime (2 * 60 * 60) payout.createdAt
  unless (now < cutoffTime) $
    throwError $ InvalidRequest "Cancel window expired (2 hours)"

  -- Update status to CANCELLED with reason
  QSP.updateStatusWithReasonByRideId DSP.CANCELLED (Just req.reason) rideId

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

  -- Validation: Can only retry FAILED payouts
  unless (payout.status == DSP.FAILED) $
    throwError $ InvalidRequest "Can only retry FAILED payouts"

  -- Validation: Can only retry once (retryCount must be Nothing or 0)
  let currentRetryCount = fromMaybe 0 payout.retryCount
  unless (currentRetryCount == 0) $
    throwError $ InvalidRequest "Retry limit exceeded (max 1 retry)"

  -- Increment retry count
  QSP.incrementRetryCountByRideId (Just 1) rideId

  -- TODO: Execute actual payout logic here with Redis lock
  -- For now, just mark as PROCESSING and then PROCESSED (placeholder)
  logInfo $ "Retrying payout for rideId: " <> rideId

  -- Mark as PROCESSING
  QSP.updateStatusWithReasonByRideId DSP.PROCESSING Nothing rideId

  -- TODO: Call actual payout service and handle success/failure
  -- For now, mark as PROCESSED (placeholder for actual implementation)
  QSP.updateStatusWithReasonByRideId DSP.PROCESSED Nothing rideId

  logInfo $ "Payout retry completed for rideId: " <> rideId
  pure Kernel.Types.APISuccess.Success

-- Helper to convert domain status to API status
convertStatus :: DSP.ScheduledPayoutStatus -> API.PayoutStatus
convertStatus DSP.PENDING = API.PENDING
convertStatus DSP.PROCESSING = API.PROCESSING
convertStatus DSP.PROCESSED = API.PROCESSED
convertStatus DSP.FAILED = API.FAILED
convertStatus DSP.CANCELLED = API.CANCELLED
