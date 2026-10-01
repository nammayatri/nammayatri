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
    getEndRideRequirements,
    requestManualTollChargeApproval,
    applyTollChargeApprovalDecision,
  )
where

import Data.OpenApi.Internal.Schema (ToSchema)
import qualified Domain.Action.UI.Ride.EndRide as RideEnd
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
import qualified SharedLogic.TollChargeDecision as TollChargeDecision
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
    approvalMode :: ManualChargeApprovalMode,
    approvalStatus :: Maybe ManualTollChargeApprovalStatus,
    requestedAmount :: Maybe HighPrecMoney,
    timeoutSeconds :: Int
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
    amount :: HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | The driver app sends its current location, the real drop point, so the preview here uses the
-- same pickup/drop threshold check end-ride itself will use. An older app that doesn't send one yet
-- falls back to assuming the route was as expected, same as this endpoint always did before.
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
  let mbMaxAllowedAmount = manualTollChargeMaxAmount thresholdConfig ride.estimatedTollCharges
  case mbMaxAllowedAmount of
    Just maxAllowedAmount | ride.status == DRide.INPROGRESS && isManualTollChargeEnabled thresholdConfig booking.tripCategory && not (DTC.isTollExemptVehicleTier booking.vehicleServiceTier) -> do
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
      let decision =
            TollChargeDecision.resolveTollChargeAndConfidence
              TollChargeDecision.TollChargeDecisionInput
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
                  enableEstimatedTollFallback = thresholdConfig.enableEstimatedTollFallback
                }
      if isTollDeclarationPanelNeeded (DTC.isTollApplicableForTrip booking.vehicleServiceTier booking.tripCategory) decision
        then do
          approvalMode <- getApprovalMode ride.id booking.id.getId
          mbRecord :: Maybe TollChargeApprovalRecord <- Redis.safeGet (tollChargeApprovalRecordKey ride.id)
          now <- getCurrentTime
          let mbApprovalStatus = case approvalMode of
                CUSTOMER_APPROVAL -> effectiveApprovalStatus now <$> mbRecord
                DRIVER_DECLARATION_WITH_CAP -> Nothing
          pure
            EndRideRequirementsRes
              { manualTollCharge =
                  Just
                    ManualTollChargeRequirement
                      { tollNames = decision.tollNames <|> ride.estimatedTollNames,
                        suggestedAmount = decision.tollCharges <|> ride.estimatedTollCharges,
                        maxAllowedAmount = maxAllowedAmount,
                        currency = ride.currency,
                        approvalMode = approvalMode,
                        approvalStatus = mbApprovalStatus,
                        requestedAmount = (.amount) <$> mbRecord,
                        timeoutSeconds = approvalTimeoutSeconds thresholdConfig
                      }
              }
        else pure EndRideRequirementsRes {manualTollCharge = Nothing}
    _ -> pure EndRideRequirementsRes {manualTollCharge = Nothing}

-- | The driver has entered a toll amount that needs the rider's approval. The BAP shows the prompt to
-- the rider. The driver app then polls getEndRideRequirements for the outcome and ends the ride with
-- the approved amount.
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
  maxAllowedAmount <- manualTollChargeMaxAmount thresholdConfig ride.estimatedTollCharges & fromMaybeM ManualTollChargeNotAllowed
  when (req.amount < 0 || req.amount > maxAllowedAmount) $ throwError (ManualTollChargeAboveLimit maxAllowedAmount)
  approvalMode <- getApprovalMode ride.id booking.id.getId
  unless (approvalMode == CUSTOMER_APPROVAL) $ throwError ManualTollChargeNotAllowed
  now <- getCurrentTime
  if req.amount == 0
    then -- A declared 0 needs nobody's approval: resolve it immediately instead of bothering the
    -- rider with a request for a toll that was never actually charged.
      Redis.setExp (tollChargeApprovalRecordKey ride.id) (TollChargeApprovalRecord TOLL_CHARGE_APPROVED_BY_CUSTOMER 0 now (approvalTimeoutSeconds thresholdConfig)) tollChargeApprovalRedisTtlSec
    else do
      mbRecord :: Maybe TollChargeApprovalRecord <- Redis.safeGet (tollChargeApprovalRecordKey ride.id)
      let isAlreadyRequested = case mbRecord of
            Just record -> record.amount == req.amount && effectiveApprovalStatus now record /= TOLL_CHARGE_REJECTED_BY_CUSTOMER
            Nothing -> False
      unless isAlreadyRequested $ do
        Redis.setExp (tollChargeApprovalRecordKey ride.id) (TollChargeApprovalRecord TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL req.amount now (approvalTimeoutSeconds thresholdConfig)) tollChargeApprovalRedisTtlSec
        appBackendBapInternal <- asks (.appBackendBapInternal)
        requestResult <-
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
                  approvalTimeoutSeconds = approvalTimeoutSeconds thresholdConfig
                }
        whenLeft requestResult $ \err -> do
          logError $ "Toll charge approval request failed for ride " <> ride.id.getId <> ": " <> show err
          Redis.del (tollChargeApprovalRecordKey ride.id)
          throwError TollChargeApprovalRequestFailed
  pure Success

-- | Called through the internal endpoint the BAP uses to pass on the rider's decision.
applyTollChargeApprovalDecision :: Id DRide.Ride -> TollChargeApprovalDecisionReq -> Maybe Text -> Flow APISuccess
applyTollChargeApprovalDecision rideId req apiKey = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- QM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  record :: TollChargeApprovalRecord <- Redis.safeGet (tollChargeApprovalRecordKey rideId) >>= fromMaybeM TollChargeApprovalNotPending
  now <- getCurrentTime
  unless (effectiveApprovalStatus now record == TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL && record.amount == req.amount) $ throwError TollChargeApprovalNotPending
  let decidedStatus = if req.approved then TOLL_CHARGE_APPROVED_BY_CUSTOMER else TOLL_CHARGE_REJECTED_BY_CUSTOMER
  Redis.setExp (tollChargeApprovalRecordKey rideId) record {status = decidedStatus} tollChargeApprovalRedisTtlSec
  pure Success
