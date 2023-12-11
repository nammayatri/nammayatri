{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Merchant.TransporterConfig where

import qualified Data.Aeson as A
import qualified Database.Beam as B
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Common
import Tools.Beam.UtilsTH

data TransporterConfigT f = TransporterConfigT
  { merchantId :: B.C f Text,
    merchantOperatingCityId :: B.C f Text,
    pickupLocThreshold :: B.C f Meters,
    dropLocThreshold :: B.C f Meters,
    rideTimeEstimatedThreshold :: B.C f Seconds,
    includeDriverCurrentlyOnRide :: B.C f Bool,
    defaultPopupDelay :: B.C f Seconds,
    popupDelayToAddAsPenalty :: B.C f (Maybe Seconds),
    thresholdCancellationScore :: B.C f (Maybe Int),
    minRidesForCancellationScore :: B.C f (Maybe Int),
    thresholdCancellationPercentageToUnlist :: B.C f (Maybe Int),
    minRidesToUnlist :: B.C f (Maybe Int),
    mediaFileUrlPattern :: B.C f Text,
    mediaFileSizeUpperLimit :: B.C f Int,
    referralLinkPassword :: B.C f Text,
    fcmUrl :: B.C f Text,
    fcmServiceAccount :: B.C f Text,
    fcmTokenKeyPrefix :: B.C f Text,
    onboardingTryLimit :: B.C f Int,
    onboardingRetryTimeInHours :: B.C f Int,
    checkImageExtractionForDashboard :: B.C f Bool,
    searchRepeatLimit :: B.C f Int,
    actualRideDistanceDiffThreshold :: B.C f HighPrecMeters,
    upwardsRecomputeBuffer :: B.C f HighPrecMeters,
    approxRideDistanceDiffThreshold :: B.C f HighPrecMeters,
    driverPaymentCycleBuffer :: B.C f Seconds,
    driverPaymentCycleDuration :: B.C f Seconds,
    driverPaymentCycleStartTime :: B.C f Seconds,
    driverPaymentReminderInterval :: B.C f Seconds,
    driverAutoPayNotificationTime :: B.C f Seconds,
    driverAutoPayExecutionTime :: B.C f Seconds,
    driverFeeMandateNotificationBatchSize :: B.C f Int,
    driverFeeMandateExecutionBatchSize :: B.C f Int,
    mandateNotificationRescheduleInterval :: B.C f Seconds,
    mandateExecutionRescheduleInterval :: B.C f Seconds,
    driverFeeCalculationTime :: B.C f (Maybe Seconds),
    driverFeeCalculatorBatchSize :: B.C f (Maybe Int),
    driverFeeCalculatorBatchGap :: B.C f (Maybe Seconds),
    rcLimit :: B.C f Int,
    automaticRCActivationCutOff :: B.C f Seconds,
    orderAndNotificationStatusCheckTime :: B.C f Seconds,
    isAvoidToll :: B.C f Bool,
    timeDiffFromUtc :: B.C f Seconds,
    driverFeeOverlaySendingTimeLimitInDays :: B.C f Int,
    overlayBatchSize :: B.C f Int,
    subscription :: B.C f Bool,
    minLocationAccuracy :: B.C f Double,
    aadhaarVerificationRequired :: B.C f Bool,
    enableDashboardSms :: B.C f Bool,
    subscriptionStartTime :: B.C f UTCTime,
    avgSpeedOfVehicle :: B.C f (Maybe A.Value),
    mandateValidity :: B.C f Int,
    bankErrorExpiry :: B.C f Seconds,
    driverLocationAccuracyBuffer :: B.C f Meters,
    routeDeviationThreshold :: B.C f Meters,
    canDowngradeToSedan :: B.C f Bool,
    canDowngradeToHatchback :: B.C f Bool,
    canDowngradeToTaxi :: B.C f Bool,
    canSuvDowngradeToTaxi :: B.C f Bool,
    aadhaarImageResizeConfig :: B.C f (Maybe A.Value),
    enableFaceVerification :: B.C f Bool,
    specialZoneBookingOtpExpiry :: B.C f Int,
    driverFeeRetryThresholdConfig :: B.C f Int,
    isPlanMandatory :: B.C f Bool,
    freeTrialDays :: B.C f Int,
    openMarketUnBlocked :: B.C f Bool,
    cacheOfferListByDriverId :: B.C f Bool,
    useOfferListCache :: B.C f Bool,
    updateNotificationStatusBatchSize :: B.C f Int,
    updateOrderStatusBatchSize :: B.C f Int,
    orderAndNotificationStatusCheckTimeLimit :: B.C f Seconds,
    ratingAsDecimal :: B.C f Bool,
    refillVehicleModel :: B.C f Bool,
    snapToRoadConfidenceThreshold :: B.C f Double,
    useWithSnapToRoadFallback :: B.C f Bool,
    volunteerSmsSendingLimit :: B.C f (Maybe A.Value),
    driverSmsReceivingLimit :: B.C f (Maybe A.Value),
    languagesToBeTranslated :: B.C f [Language],
    coinFeature :: B.C f Bool,
    coinConversionRate :: B.C f HighPrecMoney,
    cancellationTimeDiff :: B.C f Seconds,
    coinExpireTime :: B.C f Seconds,
    stepFunctionToConvertCoins :: B.C f Int,
    cancellationDistDiff :: B.C f Int,
    considerSpecialZoneRidesForPlanCharges :: B.C f Bool,
    considerSpecialZoneRideChargesInFreeTrial :: B.C f Bool,
    enableUdfForOffers :: B.C f Bool,
    nightSafetyRouteDeviationThreshold :: B.C f Meters,
    nightSafetyStartTime :: B.C f Seconds,
    nightSafetyEndTime :: B.C f Seconds,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TransporterConfigT where
  data PrimaryKey TransporterConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantOperatingCityId

type TransporterConfig = TransporterConfigT Identity

$(enableKVPG ''TransporterConfigT ['merchantOperatingCityId] [])

$(mkTableInstancesWithTModifier ''TransporterConfigT "transporter_config" [("automaticRCActivationCutOff", "automatic_r_c_activation_cut_off")])
