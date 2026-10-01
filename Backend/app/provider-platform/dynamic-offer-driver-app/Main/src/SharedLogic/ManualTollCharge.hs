{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.ManualTollCharge
  ( ManualChargeApprovalMode (..),
    ManualTollChargeApprovalStatus (..),
    ManualTollChargeConfirmedBy (..),
    ManualTollChargeResolution (..),
    TollChargeApprovalRecord (..),
    isManualTollChargeEnabled,
    manualTollChargeMaxAmount,
    isTollDeclarationPanelNeeded,
    tollChargeApprovalRecordKey,
    tollChargeApprovalRedisTtlSec,
    approvalTimeoutSeconds,
    effectiveApprovalStatus,
    getApprovalMode,
    resolveManualTollChargeForEndRide,
    manualTollChargeTag,
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
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Confidence
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import SharedLogic.TollChargeDecision (TollChargeDecision (..))
import Tools.Error
import Tools.Metrics (CoreMetrics)

data ManualChargeApprovalMode = DRIVER_DECLARATION_WITH_CAP | CUSTOMER_APPROVAL
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data ManualTollChargeApprovalStatus
  = TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL
  | TOLL_CHARGE_APPROVED_BY_CUSTOMER
  | TOLL_CHARGE_REJECTED_BY_CUSTOMER
  | TOLL_CHARGE_AUTO_APPROVED
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

-- ConfirmedByNoTollDeclared is its own case, not ConfirmedByDriver, so a declared amount of 0 is
-- never indistinguishable on the ride's tags from an ordinary declaration-mode confirmation --
-- including when it bypassed a customer-approval requirement that would otherwise have applied.
data ManualTollChargeConfirmedBy = ConfirmedByDriver | ConfirmedByCustomer | ConfirmedByTimeout | ConfirmedByNoTollDeclared
  deriving (Generic, Show, Eq)

data ManualTollChargeResolution = ManualTollChargeResolution
  { amount :: HighPrecMoney,
    confirmedBy :: ManualTollChargeConfirmedBy
  }

data TollChargeApprovalRecord = TollChargeApprovalRecord
  { status :: ManualTollChargeApprovalStatus,
    amount :: HighPrecMoney,
    requestedAt :: UTCTime,
    timeoutSeconds :: Int
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
  maybe False (tripCategoryName tripCategory `elem`) transporterConfig.manualTollChargeTripCategories

-- The estimated toll caps the declaration when there is one. Otherwise the configured maximum
-- applies, and without it no declaration is allowed.
manualTollChargeMaxAmount :: DTConf.TransporterConfig -> Maybe HighPrecMoney -> Maybe HighPrecMoney
manualTollChargeMaxAmount transporterConfig mbEstimatedTollCharges = case mbEstimatedTollCharges of
  Just estimatedTollCharges | estimatedTollCharges > 0 -> Just estimatedTollCharges
  _ -> transporterConfig.manualTollChargeMaxAmountWithoutEstimate

-- Trips where toll is not detected at all always get the panel. Trips where it is detected get it
-- only when the detection outcome is not a clean Sure.
isTollDeclarationPanelNeeded :: Bool -> TollChargeDecision -> Bool
isTollDeclarationPanelNeeded isTollApplicableTrip decision = case decision.tollConfidence of
  Just Sure -> decision.hasNoTollEvidence
  Just _ -> True
  Nothing -> not isTollApplicableTrip

tollChargeApprovalRecordKey :: Id DRide.Ride -> Text
tollChargeApprovalRecordKey rideId = "TollChargeApproval:RideId-" <> rideId.getId

tollChargeApprovalModeKey :: Id DRide.Ride -> Text
tollChargeApprovalModeKey rideId = "TollChargeApprovalMode:RideId-" <> rideId.getId

tollChargeApprovalRedisTtlSec :: Int
tollChargeApprovalRedisTtlSec = 21600

approvalTimeoutSeconds :: DTConf.TransporterConfig -> Int
approvalTimeoutSeconds transporterConfig = fromMaybe 180 transporterConfig.manualTollChargeApprovalTimeoutSeconds

-- A request nobody answered within the timeout it was made with reads as approved. The timeout is
-- pinned on the record at request time rather than re-read from live config, so a config change
-- mid-flight can't make the BPP and BAP disagree on whether a pending request has expired.
effectiveApprovalStatus :: UTCTime -> TollChargeApprovalRecord -> ManualTollChargeApprovalStatus
effectiveApprovalStatus now record
  | record.status == TOLL_CHARGE_PENDING_CUSTOMER_APPROVAL && diffUTCTime now record.requestedAt > fromIntegral record.timeoutSeconds = TOLL_CHARGE_AUTO_APPROVED
  | otherwise = record.status

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
  m ManualChargeApprovalMode
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
          Redis.setExp (tollChargeApprovalModeKey rideId) approvalMode tollChargeApprovalRedisTtlSec
          pure approvalMode
        Left err -> do
          logWarning $ "Could not get toll charge approval mode for ride " <> rideId.getId <> ": " <> show err
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
  TollChargeDecision ->
  HighPrecMoney ->
  m ManualTollChargeResolution
resolveManualTollChargeForEndRide transporterConfig ride tripCategory vehicleServiceTier bppBookingId autoDecision declaredAmount = do
  unless (isManualTollChargeEnabled transporterConfig tripCategory) $ throwError ManualTollChargeNotAllowed
  when (DTC.isTollExemptVehicleTier vehicleServiceTier) $ throwError ManualTollChargeNotAllowed
  -- The client is only ever supposed to ask for a declaration when the automatic decision itself
  -- wasn't a genuine Sure; re-check that here instead of trusting a client-side skip.
  when (autoDecision.tollConfidence == Just Sure && not autoDecision.hasNoTollEvidence) $ throwError ManualTollChargeNotAllowed
  maxAmount <- manualTollChargeMaxAmount transporterConfig ride.estimatedTollCharges & fromMaybeM ManualTollChargeNotAllowed
  when (declaredAmount < 0 || declaredAmount > maxAmount) $ throwError (ManualTollChargeAboveLimit maxAmount)
  if declaredAmount == 0
    then -- A declared 0 needs nobody's approval, in either mode, and regardless of whatever
    -- request may already be sitting against this ride.
      pure ManualTollChargeResolution {amount = 0, confirmedBy = ConfirmedByNoTollDeclared}
    else do
      approvalMode <- getApprovalMode ride.id bppBookingId
      case approvalMode of
        DRIVER_DECLARATION_WITH_CAP -> pure ManualTollChargeResolution {amount = declaredAmount, confirmedBy = ConfirmedByDriver}
        CUSTOMER_APPROVAL -> do
          mbRecord :: Maybe TollChargeApprovalRecord <- Redis.safeGet (tollChargeApprovalRecordKey ride.id)
          now <- getCurrentTime
          case mbRecord of
            Just record | record.amount == declaredAmount -> case effectiveApprovalStatus now record of
              TOLL_CHARGE_APPROVED_BY_CUSTOMER -> pure ManualTollChargeResolution {amount = declaredAmount, confirmedBy = ConfirmedByCustomer}
              TOLL_CHARGE_AUTO_APPROVED -> pure ManualTollChargeResolution {amount = declaredAmount, confirmedBy = ConfirmedByTimeout}
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

isManualTollChargeRide :: DRide.Ride -> Bool
isManualTollChargeRide ride = any (\tag -> manualTollChargeTagPrefix `T.isPrefixOf` LYT.getTagNameValue tag) (fromMaybe [] ride.rideTags)
