{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Merchant.TransporterConfig where

import Data.Time (NominalDiffTime, UTCTime)
import Domain.Types.Common
import Domain.Types.Location (DummyLocationInfo)
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import EulerHS.Prelude hiding (id)
import Kernel.External.Notification.FCM.Types (FCMConfig)
import Kernel.External.Types (Language)
import Kernel.Types.Common
import Kernel.Types.Id

data AadhaarImageResizeConfig = AadhaarImageResizeConfig
  { height :: Int,
    width :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data AvgSpeedOfVechilePerKm = AvgSpeedOfVechilePerKm -- FIXME make datatype to [(Variant, Kilometers)]
  { sedan :: Kilometers,
    suv :: Kilometers,
    hatchback :: Kilometers,
    autorickshaw :: Kilometers,
    taxi :: Kilometers,
    taxiplus :: Kilometers
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data DashboardMediaSendingLimit = DashboardMediaSendingLimit
  { sms :: Int,
    whatsapp :: Int,
    overlay :: Int,
    alert :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- ProviderConfig?
data TransporterConfigD u = TransporterConfig
  { merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    pickupLocThreshold :: Meters,
    dropLocThreshold :: Meters,
    rideTimeEstimatedThreshold :: Seconds,
    includeDriverCurrentlyOnRide :: Bool,
    defaultPopupDelay :: Seconds,
    popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    minRidesForCancellationScore :: Maybe Int,
    thresholdCancellationPercentageToUnlist :: Maybe Int,
    minRidesToUnlist :: Maybe Int,
    mediaFileUrlPattern :: Text,
    mediaFileSizeUpperLimit :: Int,
    referralLinkPassword :: Text,
    fcmConfig :: FCMConfig,
    onboardingTryLimit :: Int,
    onboardingRetryTimeInHours :: Int,
    checkImageExtractionForDashboard :: Bool,
    searchRepeatLimit :: Int,
    actualRideDistanceDiffThreshold :: HighPrecMeters,
    upwardsRecomputeBuffer :: HighPrecMeters,
    approxRideDistanceDiffThreshold :: HighPrecMeters,
    minLocationAccuracy :: Double,
    driverPaymentCycleBuffer :: NominalDiffTime,
    driverPaymentCycleDuration :: NominalDiffTime,
    driverPaymentCycleStartTime :: NominalDiffTime,
    driverPaymentReminderInterval :: NominalDiffTime,
    driverAutoPayNotificationTime :: NominalDiffTime,
    driverAutoPayExecutionTime :: NominalDiffTime,
    bankErrorExpiry :: NominalDiffTime,
    driverFeeMandateNotificationBatchSize :: Int,
    driverFeeMandateExecutionBatchSize :: Int,
    mandateNotificationRescheduleInterval :: NominalDiffTime,
    mandateExecutionRescheduleInterval :: NominalDiffTime,
    driverFeeCalculationTime :: Maybe NominalDiffTime,
    driverFeeCalculatorBatchSize :: Maybe Int,
    driverFeeCalculatorBatchGap :: Maybe NominalDiffTime,
    driverFeeRetryThresholdConfig :: Int,
    orderAndNotificationStatusCheckTime :: NominalDiffTime,
    orderAndNotificationStatusCheckTimeLimit :: NominalDiffTime,
    badDebtBatchSize :: Int,
    badDebtRescheduleTime :: NominalDiffTime,
    badDebtSchedulerTime :: NominalDiffTime,
    badDebtTimeThreshold :: Int,
    timeDiffFromUtc :: Seconds,
    subscription :: Bool,
    subscriptionStartTime :: UTCTime,
    avgSpeedOfVehicle :: Maybe AvgSpeedOfVechilePerKm,
    updateNotificationStatusBatchSize :: Int,
    updateOrderStatusBatchSize :: Int,
    mandateValidity :: Int,
    aadhaarVerificationRequired :: Bool,
    enableDashboardSms :: Bool,
    driverLocationAccuracyBuffer :: Meters,
    routeDeviationThreshold :: Meters,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    canDowngradeToTaxi :: Bool,
    canSuvDowngradeToTaxi :: Bool,
    rcLimit :: Int,
    automaticRCActivationCutOff :: Seconds,
    languagesToBeTranslated :: [Language],
    isAvoidToll :: Bool,
    aadhaarImageResizeConfig :: Maybe AadhaarImageResizeConfig,
    enableFaceVerification :: Bool,
    specialZoneBookingOtpExpiry :: Int,
    isPlanMandatory :: Bool,
    freeTrialDays :: Int,
    openMarketUnBlocked :: Bool,
    cacheOfferListByDriverId :: Bool,
    useOfferListCache :: Bool,
    ratingAsDecimal :: Bool,
    coinFeature :: Bool,
    coinConversionRate :: HighPrecMoney,
    refillVehicleModel :: Bool,
    driverFeeOverlaySendingTimeLimitInDays :: Int,
    overlayBatchSize :: Int,
    snapToRoadConfidenceThreshold :: Double,
    useWithSnapToRoadFallback :: Bool,
    volunteerSmsSendingLimit :: Maybe DashboardMediaSendingLimit,
    driverSmsReceivingLimit :: Maybe DashboardMediaSendingLimit,
    cancellationTimeDiff :: NominalDiffTime,
    coinExpireTime :: NominalDiffTime,
    stepFunctionToConvertCoins :: Int,
    cancellationDistDiff :: Int,
    considerSpecialZoneRidesForPlanCharges :: Bool,
    considerSpecialZoneRideChargesInFreeTrial :: Bool,
    enableUdfForOffers :: Bool,
    nightSafetyRouteDeviationThreshold :: Meters,
    nightSafetyStartTime :: Seconds,
    nightSafetyEndTime :: Seconds,
    cancellationFee :: HighPrecMoney,
    driverDistanceTravelledOnPickupThresholdOnCancel :: Meters,
    driverTimeSpentOnPickupThresholdOnCancel :: Seconds,
    cancellationFeeDisputeLimit :: Int,
    driverDistanceToPickupThresholdOnCancel :: Meters,
    numOfCancellationsAllowed :: Int,
    canAddCancellationFee :: Bool,
    allowDefaultPlanAllocation :: Bool,
    specialDrivers :: [Text],
    specialLocationTags :: [Text],
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    notificationRetryEligibleErrorCodes :: [Text],
    notificationRetryCountThreshold :: Int,
    notificationRetryTimeGap :: NominalDiffTime,
    driverAutoPayExecutionTimeFallBack :: NominalDiffTime,
    orderAndNotificationStatusCheckFallBackTime :: NominalDiffTime,
    kaptureDisposition :: Text,
    dummyFromLocation :: DummyLocationInfo,
    dummyToLocation :: DummyLocationInfo
  }
  deriving (Generic, Show)

type TransporterConfig = TransporterConfigD 'Safe

instance FromJSON (TransporterConfigD 'Unsafe)

instance ToJSON (TransporterConfigD 'Unsafe)
