{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.ManualTollCharge
  ( ManualTollChargeApprovalStatus (..),
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
    resolveManualTollChargeForEndRide,
    manualTollChargeTag,
    manualTollChargeRejectionTag,
    manualTollChargeSendFailedTag,
    manualTollChargeConfidence,
    isManualTollChargeRide,
  )
where

import Data.OpenApi.Internal.Schema (ToSchema)
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
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
import Tools.Error

-- PENDING = written, not yet acknowledged by the rider app; DELIVERED = acknowledged;
-- SEND_FAILED = every try to reach the rider backend failed, so the driver can send again or end without a toll.
-- NOT_DELIVERED and AUTO_APPROVED are never stored, only derived by effectiveApprovalStatus.
data ManualTollChargeApprovalStatus
  = TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL
  | TOLL_CHARGE_DELIVERED_TO_CUSTOMER
  | TOLL_CHARGE_APPROVED_BY_CUSTOMER
  | TOLL_CHARGE_REJECTED_BY_CUSTOMER
  | TOLL_CHARGE_AUTO_APPROVED
  | TOLL_CHARGE_NOT_DELIVERED
  | TOLL_CHARGE_SEND_FAILED
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

-- Each "no charge" outcome is its own case so the tag stays distinguishable: NoTollDeclared is a
-- driver declaring 0, Unsettled is the rejection cap hit, NotDelivered is a prompt never seen.
data ManualTollChargeConfirmedBy = ConfirmedByCustomer | ConfirmedByTimeout | ConfirmedByNoTollDeclared | ConfirmedByUnsettled | ConfirmedByNotDelivered
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
  | expired && record.status `elem` [TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL, TOLL_CHARGE_SEND_FAILED] = TOLL_CHARGE_NOT_DELIVERED
  | expired && record.status == TOLL_CHARGE_DELIVERED_TO_CUSTOMER = TOLL_CHARGE_AUTO_APPROVED
  | otherwise = record.status
  where
    expired = diffUTCTime now record.requestedAt > fromIntegral record.timeoutSeconds

-- DELIVERED is still waiting on the rider, so it reads as pending to the driver app; every other
-- status, including NOT_DELIVERED, is exposed as-is.
clientVisibleApprovalStatus :: ManualTollChargeApprovalStatus -> ManualTollChargeApprovalStatus
clientVisibleApprovalStatus TOLL_CHARGE_DELIVERED_TO_CUSTOMER = TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL
clientVisibleApprovalStatus status = status

-- Nothing means the declaration is no longer eligible, so end ride bills the automatic toll
-- decision instead of failing: the client declared against an earlier requirements check, and both
-- the config and the final toll evidence can change before the ride actually ends.
resolveManualTollChargeForEndRide ::
  ( MonadFlow m,
    CacheFlow m r
  ) =>
  DTConf.TransporterConfig ->
  DRide.Ride ->
  DTC.TripCategory ->
  DTC.ServiceTierType ->
  ManualTollChargeSignal ->
  HighPrecMoney ->
  m (Maybe ManualTollChargeResolution)
resolveManualTollChargeForEndRide transporterConfig ride tripCategory vehicleServiceTier autoDecision declaredAmount =
  -- Re-checks the exact rule the requirements endpoint used instead of trusting a client-side skip.
  if isManualTollChargeEnabled transporterConfig tripCategory
    && not (DTC.isTollExemptVehicleTier vehicleServiceTier)
    && shouldRequestManualTollCharge (DTC.isTollApplicableForTrip vehicleServiceTier tripCategory) autoDecision
    then Just <$> resolveEligibleManualTollChargeForEndRide transporterConfig ride declaredAmount
    else do
      logWarning $ "Manual toll charge no longer eligible at end ride, billing automatically. RideId: " <> ride.id.getId
      pure Nothing

-- Only reached once the declaration is known to be eligible. Approval-state errors still throw,
-- since the driver has to act on them: a rejection needs a new amount, a pending one needs a wait.
resolveEligibleManualTollChargeForEndRide ::
  ( MonadFlow m,
    CacheFlow m r
  ) =>
  DTConf.TransporterConfig ->
  DRide.Ride ->
  HighPrecMoney ->
  m ManualTollChargeResolution
resolveEligibleManualTollChargeForEndRide transporterConfig ride declaredAmount = do
  maxAmount <- manualTollChargeMaxAmountWithoutEstimate transporterConfig & fromMaybeM ManualTollChargeNotAllowed
  when (declaredAmount < 0 || declaredAmount > maxAmount) $ throwError (ManualTollChargeAboveLimit maxAmount)
  -- Never deleted on decode failure, so a corrupt record can't drop the rejection history. A declared 0 bills no
  -- toll whatever the record says, so for it a failed or corrupt read must not block the end ride: it only loses
  -- the rejection history on the tags.
  mbRecord :: Maybe TollChargeState <-
    if declaredAmount == 0
      then either (const Nothing) identity <$> withTryCatch "readTollChargeStateForZeroToll" (Redis.get' (tollChargeStateKey ride.id) (throwError TollChargeApprovalRequired))
      else Redis.get' (tollChargeStateKey ride.id) (throwError TollChargeApprovalRequired)
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
        then -- A declared 0 needs nobody's approval.
          pure ManualTollChargeResolution {amount = 0, confirmedBy = ConfirmedByNoTollDeclared, rejectedAmounts}
        else case mbRecord of
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
      ConfirmedByCustomer -> "Customer"
      ConfirmedByTimeout -> "Timeout"
      ConfirmedByNoTollDeclared -> "NoTollDeclared"
      ConfirmedByUnsettled -> "Unsettled"
      ConfirmedByNotDelivered -> "NotDelivered"

-- Customer/NoTollDeclared are confirmed by a party with standing to; Timeout/Unsettled/NotDelivered are
-- all system defaults, not confirmations.
manualTollChargeConfidence :: ManualTollChargeConfirmedBy -> Confidence
manualTollChargeConfidence = \case
  ConfirmedByCustomer -> Sure
  ConfirmedByNoTollDeclared -> Sure
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

manualTollChargeSendFailedTagPrefix :: Text
manualTollChargeSendFailedTagPrefix = "TollSendFailed#"

-- One tag per send that failed after every try: the amount the driver tried to charge and when, so support
-- can verify a ticket later.
manualTollChargeSendFailedTag :: HighPrecMoney -> UTCTime -> LYT.TagNameValue
manualTollChargeSendFailedTag amount failedAt =
  LYT.TagNameValue $
    manualTollChargeSendFailedTagPrefix <> show (roundToIntegral amount :: Integer) <> "@" <> T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" failedAt)
