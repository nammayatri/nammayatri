{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.EndRideRequirements
  ( EndRideRequirementsRes (..),
    ManualTollChargeRequirement (..),
    RequestTollChargeApprovalReq (..),
    TollChargeApprovalDecisionReq (..),
    TollChargeApprovalAckReq (..),
    getEndRideRequirements,
    requestManualTollChargeApproval,
    applyTollChargeApprovalAck,
    applyTollChargeApprovalDecision,
  )
where

import Data.OpenApi.Internal.Schema (ToSchema)
import qualified Domain.Action.UI.Ride.EndRide as RideEnd
import qualified Domain.Action.UI.Ride.EndRide.RecomputeDecision as RD
import qualified Domain.Action.UI.Ride.EndRide.TollDecision as TollDecision
import qualified Domain.Types as DTC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment (Flow)
import EulerHS.Prelude
import Kernel.External.Maps.Types (LatLong)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.LocationUpdates.Internal as LocUpdInternal
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import SharedLogic.ManualTollCharge
import qualified Storage.CachedQueries.Merchant as QM
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import qualified Toll.SharedLogic.TollsDetector as TollsDetector
import Tools.Error

data ManualTollChargeRequirement = ManualTollChargeRequirement
  { tollNames :: Maybe [Text],
    suggestedAmount :: Maybe HighPrecMoney,
    maxAllowedAmount :: HighPrecMoney,
    currency :: Currency,
    approvalStatus :: Maybe ManualTollChargeApprovalStatus,
    requestedAmount :: Maybe HighPrecMoney,
    timeoutSeconds :: Int,
    -- Seconds left of the request's own window, counted on the server so the driver app needs no clock of its own.
    secondsLeft :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | manualTollCharge = Nothing means the driver can call end-ride as usual.
newtype EndRideRequirementsRes = EndRideRequirementsRes
  { manualTollCharge :: Maybe ManualTollChargeRequirement
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype RequestTollChargeApprovalReq = RequestTollChargeApprovalReq
  { amount :: HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data TollChargeApprovalDecisionReq = TollChargeApprovalDecisionReq
  { approved :: Bool,
    amount :: HighPrecMoney,
    requestId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype TollChargeApprovalAckReq = TollChargeApprovalAckReq
  { requestId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | Uses the driver app's current location for the same pickup/drop threshold check end-ride
-- itself will use; an older app with no location falls back to assuming the route was as expected.
getEndRideRequirements ::
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Id DRide.Ride ->
  Maybe LatLong ->
  Flow EndRideRequirementsRes
getEndRideRequirements (driverId, _merchantId, merchantOpCityId) rideId mbTripEndPoint = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.driverId == driverId) $ throwError NotAnExecutor
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  thresholdConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let mbMaxAllowedAmount = manualTollChargeMaxAmountWithoutEstimate thresholdConfig
  case mbMaxAllowedAmount of
    Just maxAllowedAmount | ride.status == DRide.INPROGRESS && isManualTollChargeEnabled thresholdConfig booking.tripCategory && not (DTC.isTollExemptVehicleTier booking.vehicleServiceTier) -> do
      -- Plain read: a record that fails to decode reads as absent here, and is not deleted.
      mbRecord :: Maybe TollChargeState <- Redis.get' (tollChargeStateKey ride.id) (pure ())
      case mbRecord of
        Just record -> do
          now <- getCurrentTime
          let mbApprovalStatus = Just (clientVisibleApprovalStatus (effectiveApprovalStatus now record))
          pure
            EndRideRequirementsRes
              { manualTollCharge =
                  Just
                    ManualTollChargeRequirement
                      { tollNames = record.tollNames <|> ride.estimatedTollNames,
                        suggestedAmount = Just record.amount,
                        maxAllowedAmount = maxAllowedAmount,
                        currency = ride.currency,
                        approvalStatus = mbApprovalStatus,
                        requestedAmount = Just record.amount,
                        timeoutSeconds = approvalTimeoutSeconds thresholdConfig,
                        secondsLeft = Just (max 0 (ceiling (diffUTCTime (addUTCTime (fromIntegral record.timeoutSeconds) record.requestedAt) now)))
                      }
              }
        Nothing -> do
          pickupDropOutsideOfThreshold <- case mbTripEndPoint of
            Just tripEndPoint -> RideEnd.isPickupDropOutsideOfThreshold booking ride tripEndPoint thresholdConfig
            Nothing -> pure False
          distanceCalculationFailed <- LocUpdInternal.isDistanceCalculationFailedImplementation ride.driverId
          mbValidatedPendingToll <-
            TollsDetector.checkAndValidatePendingTolls
              TollsDetector.TollTrackingSnapToRoad
              ride.driverId.getId
              ride.estimatedTollCharges
              ride.estimatedTollNames
              ride.estimatedTollIds
              ride.tollCharges
              ride.tollIds
          let tollInput =
                TollDecision.TollInput
                  { distanceCalculationFailed = distanceCalculationFailed,
                    numberOfSelfTuned = ride.numberOfSelfTuned,
                    detectedTollCharges = ride.tollCharges,
                    detectedTollNames = ride.tollNames,
                    detectedTollIds = ride.tollIds,
                    estimatedTollCharges = ride.estimatedTollCharges,
                    estimatedTollNames = ride.estimatedTollNames,
                    estimatedTollIds = ride.estimatedTollIds,
                    driverDeviatedToTollRoute = ride.driverDeviatedToTollRoute,
                    pickupDropOutsideOfThreshold = pickupDropOutsideOfThreshold,
                    validatedPendingToll = mbValidatedPendingToll,
                    enableEstimatedTollFallback = (RD.mkRecomputeConfig thresholdConfig).cfgEstimatedTollFallback
                  }
              tollBilling = TollDecision.decideTollBilling tollInput
              signal = ManualTollChargeSignal {tollConfidence = tollBilling.tollConfidence, hasNoTollEvidence = TollDecision.hasNoTollEvidence tollInput}
          if shouldRequestManualTollCharge (DTC.isTollApplicableForTrip booking.vehicleServiceTier booking.tripCategory) signal
            then
              pure
                EndRideRequirementsRes
                  { manualTollCharge =
                      Just
                        ManualTollChargeRequirement
                          { tollNames = tollBilling.tollNames <|> ride.estimatedTollNames,
                            suggestedAmount = tollBilling.tollCharges <|> ride.estimatedTollCharges,
                            maxAllowedAmount = maxAllowedAmount,
                            currency = ride.currency,
                            approvalStatus = Nothing,
                            requestedAmount = Nothing,
                            timeoutSeconds = approvalTimeoutSeconds thresholdConfig,
                            secondsLeft = Nothing
                          }
                  }
            else pure EndRideRequirementsRes {manualTollCharge = Nothing}
    _ -> pure EndRideRequirementsRes {manualTollCharge = Nothing}

-- | Sends a declared amount to the rider for approval. Never deletes the record on a failed call —
-- it stays PENDING and simply expires NOT_DELIVERED. A retry for the same amount resends the same requestId.
requestManualTollChargeApproval ::
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Id DRide.Ride ->
  RequestTollChargeApprovalReq ->
  Flow APISuccess
requestManualTollChargeApproval (driverId, _merchantId, merchantOpCityId) rideId req = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.driverId == driverId) $ throwError NotAnExecutor
  unless (ride.status == DRide.INPROGRESS) $ throwError $ RideInvalidStatus ("Toll charge approval needs a ride in progress: " <> show ride.status)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  thresholdConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (isManualTollChargeEnabled thresholdConfig booking.tripCategory) $ throwError ManualTollChargeNotAllowed
  when (DTC.isTollExemptVehicleTier booking.vehicleServiceTier) $ throwError ManualTollChargeNotAllowed
  maxAllowedAmount <- manualTollChargeMaxAmountWithoutEstimate thresholdConfig & fromMaybeM ManualTollChargeNotAllowed
  when (req.amount < 0 || req.amount > maxAllowedAmount) $ throwError (ManualTollChargeAboveLimit maxAllowedAmount)
  now <- getCurrentTime
  -- A declared 0 needs nobody's approval, so leave any existing record alone rather than overwrite
  -- it with one end-ride will never read.
  unless (req.amount == 0) $ do
    newRequestId <- generateGUIDText
    -- The lock covers only the read and write; the rider call happens outside it.
    mbRecordToSend <- withTollChargeStateLock ride.id $ do
      mbRecord <- readTollChargeState ride.id (throwError TollChargeApprovalRequestFailed)
      case mbRecord of
        -- Not yet acknowledged, or the last send failed: resend under the same requestId and deadline.
        Just record | record.amount == req.amount && effectiveApprovalStatus now record `elem` [TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL, TOLL_CHARGE_SEND_FAILED] -> do
          let resendRecord = record {status = TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL}
          Redis.setExp (tollChargeStateKey ride.id) resendRecord tollChargeStateTtlSec
          pure (Just resendRecord)
        -- Already delivered, approved, or auto-approved for this amount: nothing new to ask.
        Just record | record.amount == req.amount && effectiveApprovalStatus now record `elem` [TOLL_CHARGE_DELIVERED_TO_CUSTOMER, TOLL_CHARGE_APPROVED_BY_CUSTOMER, TOLL_CHARGE_AUTO_APPROVED] -> pure Nothing
        _ -> do
          let priorRejectionCount = maybe 0 (.rejectionCount) mbRecord
              priorRejectedAmounts = maybe [] (.rejectedAmounts) mbRecord
              attemptsExhausted = case mbRecord of
                Just record -> effectiveApprovalStatus now record == TOLL_CHARGE_REJECTED_BY_CUSTOMER && priorRejectionCount >= maxApprovalAttempts thresholdConfig
                Nothing -> False
          when attemptsExhausted $ throwError TollChargeApprovalAttemptsExhausted
          let newRecord =
                TollChargeState
                  { status = TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL,
                    amount = req.amount,
                    tollNames = ride.estimatedTollNames,
                    requestedAt = now,
                    timeoutSeconds = approvalTimeoutSeconds thresholdConfig,
                    rejectionCount = priorRejectionCount,
                    rejectedAmounts = priorRejectedAmounts,
                    requestId = newRequestId
                  }
          Redis.setExp (tollChargeStateKey ride.id) newRecord tollChargeStateTtlSec
          pure (Just newRecord)
    whenJust mbRecordToSend $ \record -> do
      appBackendBapInternal <- asks (.appBackendBapInternal)
      -- The rider app can be a moment behind the ride start, so a failed send is repeated twice. Every send carries the same request id.
      requestResult <-
        retryTollApprovalSend 3 $
          withTryCatch "requestTollChargeApproval" $
            CallBAPInternal.requestTollChargeApproval
              appBackendBapInternal.apiKey
              appBackendBapInternal.url
              booking.id.getId
              CallBAPInternal.RequestTollChargeApprovalReq
                { bppRideId = ride.id.getId,
                  tollNames = ride.estimatedTollNames,
                  amount = req.amount,
                  currency = ride.currency,
                  approvalTimeoutSeconds = approvalTimeoutSeconds thresholdConfig,
                  requestId = record.requestId,
                  requestedAt = record.requestedAt
                }
      -- Left in place on failure: marked SEND_FAILED, can't be charged, expires as NOT_DELIVERED.
      whenLeft requestResult $ \_ -> do
        logError $ "Toll charge approval request failed for ride " <> ride.id.getId <> ", amount " <> show req.amount <> ", request " <> record.requestId
        markTollSendFailed ride.id record.requestId
        recordTollSendFailure ride.id req.amount
        throwError TollChargeApprovalRequestFailed
  pure Success

-- Only a record still waiting on this very request is marked: a rider who did get the request and already
-- answered or acknowledged it, or a newer request, must not be overwritten. A failure to mark never hides the send failure.
markTollSendFailed :: Id DRide.Ride -> Text -> Flow ()
markTollSendFailed rideId requestId =
  void . withTryCatch "markTollSendFailed" $
    withTollChargeStateLock rideId $ do
      mbRecord <- readTollChargeState rideId (throwError TollChargeApprovalRequestFailed)
      whenJust mbRecord $ \record ->
        when (record.requestId == requestId && record.status == TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL) $
          Redis.setExp (tollChargeStateKey rideId) record {status = TOLL_CHARGE_SEND_FAILED} tollChargeStateTtlSec

-- Kept on the ride so support can verify a ticket later. A failure to write it never hides the send failure itself.
recordTollSendFailure :: Id DRide.Ride -> HighPrecMoney -> Flow ()
recordTollSendFailure rideId amount = do
  failedAt <- getCurrentTime
  void . withTryCatch "recordTollSendFailure" $ do
    ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
    QRide.updateRideTags (Just (fromMaybe [] ride.rideTags <> [manualTollChargeSendFailedTag amount failedAt])) ride.id

retryTollApprovalSend :: MonadFlow m => Int -> m (Either SomeException a) -> m (Either SomeException a)
retryTollApprovalSend attemptsLeft send = do
  result <- send
  case result of
    Left _ | attemptsLeft > 1 -> liftIO (threadDelay 2000000) >> retryTollApprovalSend (attemptsLeft - 1) send
    _ -> pure result

-- | The rider's app has shown the prompt. Only an acknowledged request can be charged on timeout, so
-- this is what turns a sent request into one the rider can be held to.
applyTollChargeApprovalAck :: Id DRide.Ride -> TollChargeApprovalAckReq -> Maybe Text -> Flow APISuccess
applyTollChargeApprovalAck rideId req apiKey = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- QM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  unless (ride.status == DRide.INPROGRESS) $ throwError TollChargeApprovalNotPending
  withTollChargeStateLock rideId $ do
    record <- readTollChargeState rideId (throwError TollChargeApprovalNotPending) >>= fromMaybeM TollChargeApprovalNotPending
    now <- getCurrentTime
    unless (record.requestId == req.requestId) $ throwError TollChargeApprovalNotPending
    case effectiveApprovalStatus now record of
      TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL -> Redis.setExp (tollChargeStateKey rideId) record {status = TOLL_CHARGE_DELIVERED_TO_CUSTOMER} tollChargeStateTtlSec
      -- The driver saw a failed send, but the rider backend did store the request and the rider's app showed it.
      TOLL_CHARGE_SEND_FAILED -> Redis.setExp (tollChargeStateKey rideId) record {status = TOLL_CHARGE_DELIVERED_TO_CUSTOMER} tollChargeStateTtlSec
      -- Acknowledging the same request twice is harmless.
      TOLL_CHARGE_DELIVERED_TO_CUSTOMER -> pure ()
      _ -> throwError TollChargeApprovalNotPending
  pure Success

-- | Called through the internal endpoint the BAP uses to pass on the rider's decision.
applyTollChargeApprovalDecision :: Id DRide.Ride -> TollChargeApprovalDecisionReq -> Maybe Text -> Flow APISuccess
applyTollChargeApprovalDecision rideId req apiKey = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- QM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  unless (ride.status == DRide.INPROGRESS) $ throwError TollChargeApprovalNotPending
  let decidedStatus = if req.approved then TOLL_CHARGE_APPROVED_BY_CUSTOMER else TOLL_CHARGE_REJECTED_BY_CUSTOMER
  withTollChargeStateLock rideId $ do
    record <- readTollChargeState rideId (throwError TollChargeApprovalNotPending) >>= fromMaybeM TollChargeApprovalNotPending
    now <- getCurrentTime
    unless (record.requestId == req.requestId) $ throwError TollChargeApprovalNotPending
    -- Repeating a decision already applied to this request is harmless. A different decision on a
    -- request that is no longer open is refused.
    unless (record.status == decidedStatus) $ do
      unless (effectiveApprovalStatus now record `elem` [TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL, TOLL_CHARGE_SEND_FAILED, TOLL_CHARGE_DELIVERED_TO_CUSTOMER] && record.amount == req.amount) $
        throwError TollChargeApprovalNotPending
      let updatedRejectionCount = if req.approved then record.rejectionCount else record.rejectionCount + 1
          updatedRejectedAmounts = if req.approved then record.rejectedAmounts else record.rejectedAmounts <> [record.amount]
      Redis.setExp (tollChargeStateKey rideId) record {status = decidedStatus, rejectionCount = updatedRejectionCount, rejectedAmounts = updatedRejectedAmounts} tollChargeStateTtlSec
  pure Success

-- | Runs the action under the ride's toll state lock. Bounded wait: after ~5s the caller gets a
-- retriable error instead of hanging.
withTollChargeStateLock :: Id DRide.Ride -> Flow a -> Flow a
withTollChargeStateLock rideId action = acquire (50 :: Int)
  where
    lockKey = "TollChargeStateLock:RideId-" <> rideId.getId
    acquire attemptsLeft = do
      acquired <- Redis.tryLockRedis lockKey 10
      if acquired
        then action `finally` Redis.unlockRedis lockKey
        else
          if attemptsLeft <= 0
            then throwError TollChargeApprovalRequestFailed
            else liftIO (threadDelay 100000) >> acquire (attemptsLeft - 1)

-- | Reads the record without deleting it. A record that fails to decode is treated as the failure
-- given, not as absent, so a corrupt record can never reset the rejection count.
readTollChargeState :: Id DRide.Ride -> Flow () -> Flow (Maybe TollChargeState)
readTollChargeState rideId = Redis.get' (tollChargeStateKey rideId)
