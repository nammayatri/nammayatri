{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TransporterConfig (module Domain.Types.TransporterConfig, module ReExport) where

import Data.Aeson
import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Extra.TransporterConfig as ReExport
import qualified Domain.Types.Extra.TransporterConfig
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Vehicle
import qualified Email.Types
import qualified Kernel.External.Notification.FCM.Types
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Tools.Beam.UtilsTH

data TransporterConfigD (s :: UsageSafety) = TransporterConfig
  { aadhaarImageResizeConfig :: Kernel.Prelude.Maybe Domain.Types.TransporterConfig.AadhaarImageResizeConfig,
    aadhaarVerificationRequired :: Kernel.Prelude.Bool,
    acStatusCheckGap :: Kernel.Prelude.Int,
    actualRideDistanceDiffThreshold :: Kernel.Types.Common.HighPrecMeters,
    actualRideDistanceDiffThresholdIfWithinPickupDrop :: Kernel.Types.Common.HighPrecMeters,
    allowDefaultPlanAllocation :: Kernel.Prelude.Bool,
    approxRideDistanceDiffThreshold :: Kernel.Types.Common.HighPrecMeters,
    arrivedPickupThreshold :: Kernel.Types.Common.HighPrecMeters,
    arrivedStopThreshold :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    arrivingPickupThreshold :: Kernel.Types.Common.HighPrecMeters,
    automaticRCActivationCutOff :: Kernel.Types.Common.Seconds,
    avgSpeedOfVehicle :: Kernel.Prelude.Maybe Domain.Types.TransporterConfig.AvgSpeedOfVechilePerKm,
    badDebtBatchSize :: Kernel.Prelude.Int,
    badDebtRescheduleTime :: Kernel.Prelude.NominalDiffTime,
    badDebtSchedulerTime :: Kernel.Prelude.NominalDiffTime,
    badDebtTimeThreshold :: Kernel.Prelude.Int,
    bankErrorExpiry :: Kernel.Prelude.NominalDiffTime,
    bookAnyVehicleDowngradeLevel :: Kernel.Prelude.Int,
    cacheOfferListByDriverId :: Kernel.Prelude.Bool,
    cachedDevicesOSForSearchRequest :: [Kernel.Types.Version.DeviceType],
    canAddCancellationFee :: Kernel.Prelude.Bool,
    canDowngradeToHatchback :: Kernel.Prelude.Bool,
    canDowngradeToSedan :: Kernel.Prelude.Bool,
    canDowngradeToTaxi :: Kernel.Prelude.Bool,
    canSuvDowngradeToHatchback :: Kernel.Prelude.Bool,
    canSuvDowngradeToTaxi :: Kernel.Prelude.Bool,
    canSwitchToInterCity :: Kernel.Prelude.Bool,
    canSwitchToRental :: Kernel.Prelude.Bool,
    cancellationDistDiff :: Kernel.Prelude.Int,
    cancellationFee :: Kernel.Types.Common.HighPrecMoney,
    cancellationFeeDisputeLimit :: Kernel.Prelude.Int,
    cancellationTimeDiff :: Kernel.Prelude.NominalDiffTime,
    checkImageExtractionForDashboard :: Kernel.Prelude.Bool,
    coinConversionRate :: Kernel.Types.Common.HighPrecMoney,
    coinExpireTime :: Kernel.Prelude.NominalDiffTime,
    coinFeature :: Kernel.Prelude.Bool,
    considerDriversForSearch :: Kernel.Prelude.Bool,
    considerSpecialZoneRideChargesInFreeTrial :: Kernel.Prelude.Bool,
    considerSpecialZoneRidesForPlanCharges :: Kernel.Prelude.Bool,
    createDocumentRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    crossTravelCities :: [Kernel.Types.Beckn.City.City],
    currency :: Kernel.Types.Common.Currency,
    defaultPopupDelay :: Kernel.Types.Common.Seconds,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    dlNumberVerification :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    driverAutoPayExecutionTime :: Kernel.Prelude.NominalDiffTime,
    driverAutoPayExecutionTimeFallBack :: Kernel.Prelude.NominalDiffTime,
    driverAutoPayNotificationTime :: Kernel.Prelude.NominalDiffTime,
    driverDistanceToPickupThresholdOnCancel :: Kernel.Types.Common.Meters,
    driverDistanceTravelledOnPickupThresholdOnCancel :: Kernel.Types.Common.Meters,
    driverFeeCalculationTime :: Kernel.Prelude.Maybe Kernel.Prelude.NominalDiffTime,
    driverFeeCalculatorBatchGap :: Kernel.Prelude.Maybe Kernel.Prelude.NominalDiffTime,
    driverFeeCalculatorBatchSize :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    driverFeeMandateExecutionBatchSize :: Kernel.Prelude.Int,
    driverFeeMandateNotificationBatchSize :: Kernel.Prelude.Int,
    driverFeeOverlaySendingTimeLimitInDays :: Kernel.Prelude.Int,
    driverFeeRetryThresholdConfig :: Kernel.Prelude.Int,
    driverLocationAccuracyBuffer :: Kernel.Types.Common.Meters,
    driverPaymentCycleBuffer :: Kernel.Prelude.NominalDiffTime,
    driverPaymentCycleDuration :: Kernel.Prelude.NominalDiffTime,
    driverPaymentCycleStartTime :: Kernel.Prelude.NominalDiffTime,
    driverPaymentReminderInterval :: Kernel.Prelude.NominalDiffTime,
    driverSmsReceivingLimit :: Kernel.Prelude.Maybe Domain.Types.TransporterConfig.DashboardMediaSendingLimit,
    driverTimeSpentOnPickupThresholdOnCancel :: Kernel.Types.Common.Seconds,
    dropLocThreshold :: Kernel.Types.Common.Meters,
    dummyFromLocation :: Domain.Types.Location.DummyLocationInfo,
    dummyShowDriverAdditions :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    dummyToLocation :: Domain.Types.Location.DummyLocationInfo,
    editLocDriverPermissionNeeded :: Kernel.Prelude.Bool,
    editLocTimeThreshold :: Kernel.Types.Common.Seconds,
    emailOtpConfig :: Kernel.Prelude.Maybe Email.Types.EmailOTPConfig,
    enableDashboardSms :: Kernel.Prelude.Bool,
    enableFaceVerification :: Kernel.Prelude.Bool,
    enableTollCrossedNotifications :: Kernel.Prelude.Bool,
    enableUdfForOffers :: Kernel.Prelude.Bool,
    exotelAppIdMapping :: Kernel.Prelude.Maybe Domain.Types.Extra.TransporterConfig.ExotelMapping,
    exotelStatusCheckSchedulerDelay :: Kernel.Prelude.Int,
    fakeOtpEmails :: [Kernel.Prelude.Text],
    fakeOtpMobileNumbers :: [Kernel.Prelude.Text],
    fareRecomputeDailyExtraKmsThreshold :: Kernel.Types.Common.HighPrecMeters,
    fareRecomputeWeeklyExtraKmsThreshold :: Kernel.Types.Common.HighPrecMeters,
    fcmConfig :: Kernel.External.Notification.FCM.Types.FCMConfig,
    freeTrialDays :: Kernel.Prelude.Int,
    graceTimeForScheduledRidePickup :: Kernel.Prelude.NominalDiffTime,
    includeDriverCurrentlyOnRide :: Kernel.Prelude.Bool,
    isAvoidToll :: Kernel.Prelude.Bool,
    isDeviceIdChecksRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isPlanMandatory :: Kernel.Prelude.Bool,
    kaptureDisposition :: Kernel.Prelude.Text,
    kaptureQueue :: Kernel.Prelude.Text,
    languagesToBeTranslated :: [Kernel.External.Types.Language],
    lastNdaysToCheckForPayoutOrderStatus :: Kernel.Prelude.Int,
    mandateExecutionRescheduleInterval :: Kernel.Prelude.NominalDiffTime,
    mandateNotificationRescheduleInterval :: Kernel.Prelude.NominalDiffTime,
    mandateValidity :: Kernel.Prelude.Int,
    maxPayoutReferralForADay :: Kernel.Prelude.Int,
    mediaFileSizeUpperLimit :: Kernel.Prelude.Int,
    mediaFileUrlPattern :: Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    minLocationAccuracy :: Kernel.Prelude.Double,
    minPickupDistanceThresholdForReferralPayout :: Kernel.Types.Common.Meters,
    minRideDistanceThresholdForReferralPayout :: Kernel.Types.Common.HighPrecMeters,
    minRidesForCancellationScore :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minRidesToUnlist :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minmRentalAndScheduledBookingLeadTimeHours :: Kernel.Types.Common.Hours,
    nightSafetyEndTime :: Kernel.Types.Common.Seconds,
    nightSafetyRouteDeviationThreshold :: Kernel.Types.Common.Meters,
    nightSafetyStartTime :: Kernel.Types.Common.Seconds,
    notificationRetryCountThreshold :: Kernel.Prelude.Int,
    notificationRetryEligibleErrorCodes :: [Kernel.Prelude.Text],
    notificationRetryTimeGap :: Kernel.Prelude.NominalDiffTime,
    numOfCancellationsAllowed :: Kernel.Prelude.Int,
    onboardingRetryTimeInHours :: Kernel.Prelude.Int,
    onboardingTryLimit :: Kernel.Prelude.Int,
    openMarketUnBlocked :: Kernel.Prelude.Bool,
    orderAndNotificationStatusCheckFallBackTime :: Kernel.Prelude.NominalDiffTime,
    orderAndNotificationStatusCheckTime :: Kernel.Prelude.NominalDiffTime,
    orderAndNotificationStatusCheckTimeLimit :: Kernel.Prelude.NominalDiffTime,
    overlayBatchSize :: Kernel.Prelude.Int,
    pastDaysRideCounter :: Kernel.Prelude.Int,
    payoutBatchLimit :: Kernel.Prelude.Int,
    pickupLocThreshold :: Kernel.Types.Common.Meters,
    placeNameCacheExpiryDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    popupDelayToAddAsPenalty :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    ratingAsDecimal :: Kernel.Prelude.Bool,
    rcLimit :: Kernel.Prelude.Int,
    recomputeIfPickupDropNotOutsideOfThreshold :: Kernel.Prelude.Bool,
    referralLinkPassword :: Kernel.Prelude.Text,
    refillVehicleModel :: Kernel.Prelude.Bool,
    rideDurationDiffThreshold :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    rideDurationDownwardsRecomputeBuffer :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    rideDurationUpwardsRecomputeBuffer :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    rideTimeEstimatedThreshold :: Kernel.Types.Common.Seconds,
    routeDeviationThreshold :: Kernel.Types.Common.Meters,
    schedulePayoutForDay :: Kernel.Prelude.Maybe Kernel.Prelude.Integer,
    scheduleRideBufferTime :: Kernel.Prelude.NominalDiffTime,
    scheduledRideFilterExclusionThresholdHours :: Kernel.Types.Common.Hours,
    scheduledRideJobRescheduleTime :: Kernel.Prelude.NominalDiffTime,
    searchRepeatLimit :: Kernel.Prelude.Int,
    snapToRoadConfidenceThreshold :: Kernel.Prelude.Double,
    specialDrivers :: [Kernel.Prelude.Text],
    specialLocationTags :: [Kernel.Prelude.Text],
    specialZoneBookingOtpExpiry :: Kernel.Prelude.Int,
    stepFunctionToConvertCoins :: Kernel.Prelude.Int,
    subscription :: Kernel.Prelude.Bool,
    subscriptionStartTime :: Kernel.Prelude.UTCTime,
    thresholdCancellationPercentageToUnlist :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    thresholdCancellationScore :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    timeDiffFromUtc :: Kernel.Types.Common.Seconds,
    toNotifyDriverForExtraKmsLimitExceed :: Kernel.Prelude.Bool,
    updateNotificationStatusBatchSize :: Kernel.Prelude.Int,
    updateOrderStatusBatchSize :: Kernel.Prelude.Int,
    updatePayoutStatusBatchSize :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    upwardsRecomputeBuffer :: Kernel.Types.Common.HighPrecMeters,
    useOfferListCache :: Kernel.Prelude.Bool,
    useSilentFCMForForwardBatch :: Kernel.Prelude.Bool,
    useWithSnapToRoadFallback :: Kernel.Prelude.Bool,
    variantsToEnableForSubscription :: [Domain.Types.Vehicle.Variant],
    volunteerSmsSendingLimit :: Kernel.Prelude.Maybe Domain.Types.TransporterConfig.DashboardMediaSendingLimit
  }
  deriving (Generic, Show)

data AadhaarImageResizeConfig = AadhaarImageResizeConfig {height :: Kernel.Prelude.Int, width :: Kernel.Prelude.Int} deriving (Generic, Show, ToJSON, FromJSON, Read)

data AvgSpeedOfVechilePerKm = AvgSpeedOfVechilePerKm
  { ambulance :: Kernel.Types.Common.Kilometers,
    autorickshaw :: Kernel.Types.Common.Kilometers,
    bike :: Kernel.Types.Common.Kilometers,
    black :: Kernel.Types.Common.Kilometers,
    blackxl :: Kernel.Types.Common.Kilometers,
    hatchback :: Kernel.Types.Common.Kilometers,
    premiumsedan :: Kernel.Types.Common.Kilometers,
    sedan :: Kernel.Types.Common.Kilometers,
    suv :: Kernel.Types.Common.Kilometers,
    suvplus :: Kernel.Types.Common.Kilometers,
    taxi :: Kernel.Types.Common.Kilometers,
    taxiplus :: Kernel.Types.Common.Kilometers
  }
  deriving (Generic, Show, ToJSON, FromJSON, Read)

data DashboardMediaSendingLimit = DashboardMediaSendingLimit {alert :: Kernel.Prelude.Int, overlay :: Kernel.Prelude.Int, sms :: Kernel.Prelude.Int, whatsapp :: Kernel.Prelude.Int}
  deriving (Generic, Show, ToJSON, FromJSON, Read)

type TransporterConfig = TransporterConfigD 'Safe

instance FromJSON (TransporterConfigD 'Unsafe)

instance ToJSON (TransporterConfigD 'Unsafe)

instance FromJSON (TransporterConfigD 'Safe)

instance ToJSON (TransporterConfigD 'Safe)
