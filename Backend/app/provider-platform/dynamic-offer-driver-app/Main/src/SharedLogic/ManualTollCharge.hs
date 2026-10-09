{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.ManualTollCharge
  ( ManualTollChargeApprovalMode (..),
    ManualTollChargeApprovalStatus (..),
    ManualTollChargeConfirmedBy (..),
    ManualTollChargeResolution (..),
    ManualTollChargeSignal (..),
    TollChargeState (..),
    isManualTollChargeEnabled,
    manualTollChargeMaxAmountWithoutEstimate,
    shouldRequestManualTollCharge,
    tollChargeStateKey,
    tollChargeStateTtlSec,
    approvalTimeoutSeconds,
    maxApprovalAttempts,
    effectiveApprovalStatus,
    clientVisibleApprovalStatus,
    getApprovalMode,
    resolveManualTollChargeForEndRide,
    manualTollChargeTag,
    manualTollChargeRejectionTag,
    manualTollChargeConfidence,
    isManualTollChargeRide,
  )
where

import qualified Data.HashMap.Strict as HM
import Data.OpenApi.Internal.Schema (ToSchema)
import qualified Data.Text as T
import qualified Domain.Types as DTC
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTConf
import EulerHS.Prelude
import Kernel.Prelude (roundToIntegral)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Confidence
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import Tools.Error
import Tools.Metrics (CoreMetrics)

data ManualTollChargeApprovalMode = DRIVER_DECLARATION_WITH_CAP | CUSTOMER_APPROVAL
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

-- PENDING = written, not yet acknowledged by the rider app; DELIVERED = acknowledged.
-- NOT_DELIVERED and AUTO_APPROVED are never stored, only derived by effectiveApprovalStatus.
data ManualTollChargeApprovalStatus
  = TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL
  | TOLL_CHARGE_DELIVERED_TO_CUSTOMER
  | TOLL_CHARGE_APPROVED_BY_CUSTOMER
  | TOLL_CHARGE_REJECTED_BY_CUSTOMER
  | TOLL_CHARGE_AUTO_APPROVED
  | TOLL_CHARGE_NOT_DELIVERED
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

-- Each "no charge" outcome is its own case so the tag stays distinguishable: NoTollDeclared is a
-- driver declaring 0, Unsettled is the rejection cap hit, NotDelivered is a prompt never seen.
data ManualTollChargeConfirmedBy = ConfirmedByDriver | ConfirmedByCustomer | ConfirmedByTimeout | ConfirmedByNoTollDeclared | ConfirmedByUnsettled | ConfirmedByNotDelivered
  deriving (Generic, Show, Eq)

data ManualTollChargeResolution = ManualTollChargeResolution
  { amount :: HighPrecMoney,
    confirmedBy :: ManualTollChargeConfirmedBy,
    rejectedAmounts :: [HighPrecMoney]
  }

data TollChargeState = TollChargeState
  { status :: ManualTollChargeApprovalStatus,
    amount :: HighPrecMoney,
    -- Stashed at request time so a later requirements poll can answer from this record alone,
    -- without re-deriving the toll names from a fresh detection pass.
    tollNames :: Maybe [Text],
    requestedAt :: UTCTime,
    timeoutSeconds :: Int,
    -- Rejections only, carried forward across re-declarations; a timeout doesn't count.
    rejectionCount :: Int,
    -- The rejected amounts in order, for the analytics tag; rejectionCount still drives the cap.
    rejectedAmounts :: [HighPrecMoney],
    -- Echoed back on ack and decision, so a reply about an earlier request can't act on a newer one.
    requestId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

tripCategoryName :: DTC.TripCategory -> Text
tripCategoryName = \case
  DTC.OneWay _ -> "OneWay"
  DTC.Rental _ -> "Rental"
  DTC.RideShare _ -> "RideShare"
  DTC.InterCity _ _ -> "InterCity"
  DTC.CrossCity _ _ -> "CrossCity"
  DTC.Ambulance _ -> "Ambulance"
  DTC.Delivery _ -> "Delivery"
  DTC.EasyBooking _ -> "EasyBooking"
  DTC.IntercityRental _ _ -> "IntercityRental"

isManualTollChargeEnabled :: DTConf.TransporterConfig -> DTC.TripCategory -> Bool
isManualTollChargeEnabled transporterConfig tripCategory =
  maybe False (\config -> maybe False (tripCategoryName tripCategory `elem`) config.tripCategories) transporterConfig.manualTollChargeConfig

-- The configured maximum is the only cap. The estimate just prefills the suggested amount; the
-- config's presence is the single switch for the whole feature.
manualTollChargeMaxAmountWithoutEstimate :: DTConf.TransporterConfig -> Maybe HighPrecMoney
manualTollChargeMaxAmountWithoutEstimate transporterConfig = (.maxAmountWithoutEstimate) <$> transporterConfig.manualTollChargeConfig

-- The signal that decides whether the manual-charge panel is offered. hasNoTollEvidence is kept
-- separate from TollBilling since it depends on the raw inputs, not which billing branch fired.
data ManualTollChargeSignal = ManualTollChargeSignal
  { tollConfidence :: Maybe Confidence,
    hasNoTollEvidence :: Bool
  }

-- Always asks when nothing was detected. A genuine GPS-confirmed Sure skips asking, but
-- hasNoTollEvidence catches a Sure that's just a fallback from a nonzero estimate.
shouldRequestManualTollCharge :: Bool -> ManualTollChargeSignal -> Bool
shouldRequestManualTollCharge isTollApplicableTrip signal = case signal.tollConfidence of
  Just Sure -> signal.hasNoTollEvidence
  Just _ -> True
  Nothing -> not isTollApplicableTrip

tollChargeStateKey :: Id DRide.Ride -> Text
tollChargeStateKey rideId = "TollChargeState:RideId-" <> rideId.getId

tollChargeApprovalModeKey :: Id DRide.Ride -> Text
tollChargeApprovalModeKey rideId = "TollChargeApprovalMode:RideId-" <> rideId.getId

-- Short enough that a BAP outage doesn't pin declaration mode for the rest of a long ride, long
-- enough to cover the gap between the requirements check and the end-ride call that follows it.
approvalModeFallbackTtlSec :: Int
approvalModeFallbackTtlSec = 120

-- The record also carries the rejection history, so it must outlive the longest ride rather than
-- just the approval window. It is refreshed on every write.
tollChargeStateTtlSec :: Int
tollChargeStateTtlSec = 86400

approvalTimeoutSeconds :: DTConf.TransporterConfig -> Int
approvalTimeoutSeconds transporterConfig = fromMaybe 180 (transporterConfig.manualTollChargeConfig >>= (.approvalTimeoutSeconds))

-- How many times the customer can reject a declared toll before the ride is forced to end
-- unsettled instead of asking again.
maxApprovalAttempts :: DTConf.TransporterConfig -> Int
maxApprovalAttempts transporterConfig = fromMaybe 3 (transporterConfig.manualTollChargeConfig >>= (.maxApprovalAttempts))

-- Timeout is pinned on the record at request time so it can't move mid-flight. Past that time:
-- unacknowledged reads as NOT_DELIVERED (no charge), acknowledged reads as AUTO_APPROVED.
effectiveApprovalStatus :: UTCTime -> TollChargeState -> ManualTollChargeApprovalStatus
effectiveApprovalStatus now record
  | expired && record.status == TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL = TOLL_CHARGE_NOT_DELIVERED
  | expired && record.status == TOLL_CHARGE_DELIVERED_TO_CUSTOMER = TOLL_CHARGE_AUTO_APPROVED
  | otherwise = record.status
  where
    expired = diffUTCTime now record.requestedAt > fromIntegral record.timeoutSeconds

-- DELIVERED is still waiting on the rider, so it reads as pending to the driver app; every other
-- status, including NOT_DELIVERED, is exposed as-is.
clientVisibleApprovalStatus :: ManualTollChargeApprovalStatus -> ManualTollChargeApprovalStatus
clientVisibleApprovalStatus TOLL_CHARGE_DELIVERED_TO_CUSTOMER = TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL
clientVisibleApprovalStatus status = status

-- The BAP decides whether the rider's app can show the approval prompt. When it cannot be reached
-- the driver declaration path applies, so ending a ride never depends on the BAP being up.
getApprovalMode ::
  ( MonadFlow m,
    CacheFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["appBackendBapInternal" ::: CallBAPInternal.AppBackendBapInternal, "internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  Id DRide.Ride ->
  Text ->
  m ManualTollChargeApprovalMode
getApprovalMode rideId bppBookingId = do
  mbCachedMode <- Redis.safeGet (tollChargeApprovalModeKey rideId)
  case mbCachedMode of
    Just cachedMode -> pure cachedMode
    Nothing -> do
      appBackendBapInternal <- asks (.appBackendBapInternal)
      modeResponse <- withTryCatch "getTollChargeApprovalMode" $ CallBAPInternal.getTollChargeApprovalMode appBackendBapInternal.apiKey appBackendBapInternal.url bppBookingId
      case modeResponse of
        Right response -> do
          let approvalMode = if response.customerApprovalSupported then CUSTOMER_APPROVAL else DRIVER_DECLARATION_WITH_CAP
          Redis.setExp (tollChargeApprovalModeKey rideId) approvalMode tollChargeStateTtlSec
          pure approvalMode
        Left err -> do
          logWarning $ "Could not get toll charge approval mode for ride " <> rideId.getId <> ": " <> show err
          -- Cached briefly too, so a transient failure can't flip CUSTOMER_APPROVAL back in before
          -- this same end-ride attempt finishes; a later ride still re-asks the BAP.
          Redis.setExp (tollChargeApprovalModeKey rideId) DRIVER_DECLARATION_WITH_CAP approvalModeFallbackTtlSec
          pure DRIVER_DECLARATION_WITH_CAP

-- Checks a driver-submitted toll charge at end ride against the config, the cap and, when the
-- rider is able to approve, the rider's decision.
resolveManualTollChargeForEndRide ::
  ( MonadFlow m,
    CacheFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["appBackendBapInternal" ::: CallBAPInternal.AppBackendBapInternal, "internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  DTConf.TransporterConfig ->
  DRide.Ride ->
  DTC.TripCategory ->
  DTC.ServiceTierType ->
  Text ->
  ManualTollChargeSignal ->
  HighPrecMoney ->
  m ManualTollChargeResolution
resolveManualTollChargeForEndRide transporterConfig ride tripCategory vehicleServiceTier bppBookingId autoDecision declaredAmount = do
  unless (isManualTollChargeEnabled transporterConfig tripCategory) $ throwError ManualTollChargeNotAllowed
  when (DTC.isTollExemptVehicleTier vehicleServiceTier) $ throwError ManualTollChargeNotAllowed
  -- The client is only ever supposed to ask for a declaration when the panel itself would have
  -- been offered; re-check the exact same rule here instead of trusting a client-side skip.
  unless (shouldRequestManualTollCharge (DTC.isTollApplicableForTrip vehicleServiceTier tripCategory) autoDecision) $ throwError ManualTollChargeNotAllowed
  maxAmount <- manualTollChargeMaxAmountWithoutEstimate transporterConfig & fromMaybeM ManualTollChargeNotAllowed
  when (declaredAmount < 0 || declaredAmount > maxAmount) $ throwError (ManualTollChargeAboveLimit maxAmount)
  -- Never deleted on decode failure, so a corrupt record can't drop the rejection history.
  mbRecord :: Maybe TollChargeState <- Redis.get' (tollChargeStateKey ride.id) (throwError TollChargeApprovalRequired)
  now <- getCurrentTime
  let rejectedAmounts = maybe [] (.rejectedAmounts) mbRecord
  let attemptsExhausted = case mbRecord of
        Just record -> effectiveApprovalStatus now record == TOLL_CHARGE_REJECTED_BY_CUSTOMER && record.rejectionCount >= maxApprovalAttempts transporterConfig
        Nothing -> False
  if attemptsExhausted
    then -- Cap hit: ends unsettled regardless of what was declared this time.
      pure ManualTollChargeResolution {amount = 0, confirmedBy = ConfirmedByUnsettled, rejectedAmounts}
    else
      if declaredAmount == 0
        then -- A declared 0 needs nobody's approval, in either mode.
          pure ManualTollChargeResolution {amount = 0, confirmedBy = ConfirmedByNoTollDeclared, rejectedAmounts}
        else do
          approvalMode <- getApprovalMode ride.id bppBookingId
          case approvalMode of
            DRIVER_DECLARATION_WITH_CAP -> pure ManualTollChargeResolution {amount = declaredAmount, confirmedBy = ConfirmedByDriver, rejectedAmounts}
            CUSTOMER_APPROVAL -> case mbRecord of
              Just record | record.amount == declaredAmount -> case effectiveApprovalStatus now record of
                TOLL_CHARGE_APPROVED_BY_CUSTOMER -> pure ManualTollChargeResolution {amount = declaredAmount, confirmedBy = ConfirmedByCustomer, rejectedAmounts}
                TOLL_CHARGE_AUTO_APPROVED -> pure ManualTollChargeResolution {amount = declaredAmount, confirmedBy = ConfirmedByTimeout, rejectedAmounts}
                -- Never reached the rider's app, so nothing is charged, whatever the driver declared.
                TOLL_CHARGE_NOT_DELIVERED -> pure ManualTollChargeResolution {amount = 0, confirmedBy = ConfirmedByNotDelivered, rejectedAmounts}
                -- A rejection is a resolved, known outcome, distinct from still-pending, so the driver
                -- app can tell "declare a new amount" apart from "keep waiting".
                TOLL_CHARGE_REJECTED_BY_CUSTOMER -> throwError TollChargeApprovalRejected
                _ -> throwError TollChargeApprovalRequired
              _ -> throwError TollChargeApprovalRequired

manualTollChargeTagPrefix :: Text
manualTollChargeTagPrefix = "TollConfirmedBy#"

manualTollChargeTag :: ManualTollChargeConfirmedBy -> LYT.TagNameValue
manualTollChargeTag confirmedBy =
  LYT.TagNameValue $
    manualTollChargeTagPrefix <> case confirmedBy of
      ConfirmedByDriver -> "Driver"
      ConfirmedByCustomer -> "Customer"
      ConfirmedByTimeout -> "Timeout"
      ConfirmedByNoTollDeclared -> "NoTollDeclared"
      ConfirmedByUnsettled -> "Unsettled"
      ConfirmedByNotDelivered -> "NotDelivered"

-- Customer/NoTollDeclared are confirmed by a party with standing to; Driver is uncorroborated;
-- Timeout/Unsettled/NotDelivered are all system defaults, not confirmations.
manualTollChargeConfidence :: ManualTollChargeConfirmedBy -> Confidence
manualTollChargeConfidence = \case
  ConfirmedByCustomer -> Sure
  ConfirmedByNoTollDeclared -> Sure
  ConfirmedByDriver -> Neutral
  ConfirmedByTimeout -> Unsure
  ConfirmedByUnsettled -> Unsure
  ConfirmedByNotDelivered -> Unsure

isManualTollChargeRide :: DRide.Ride -> Bool
isManualTollChargeRide ride = any (\tag -> manualTollChargeTagPrefix `T.isPrefixOf` LYT.getTagNameValue tag) (fromMaybe [] ride.rideTags)

manualTollChargeRejectionTagPrefix :: Text
manualTollChargeRejectionTagPrefix = "TollRejectionAmounts#"

-- Rejected amounts only, in order; the settled amount is already on ride.tollCharges. Omitted
-- entirely when there were no rejections.
manualTollChargeRejectionTag :: [HighPrecMoney] -> Maybe LYT.TagNameValue
manualTollChargeRejectionTag [] = Nothing
manualTollChargeRejectionTag amounts = Just $ LYT.TagNameValue $ manualTollChargeRejectionTagPrefix <> T.intercalate "&" (map (show . (roundToIntegral :: HighPrecMoney -> Integer)) amounts)
