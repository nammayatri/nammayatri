{-# LANGUAGE ApplicativeDo #-}
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
import qualified Domain.Types.VehicleVariant
import qualified Email.Types
import qualified Kernel.External.Notification.FCM.Types
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified SharedLogic.BehaviourManagement.IssueBreach
import qualified Tools.Beam.UtilsTH

data TransporterConfigD (s :: UsageSafety) = TransporterConfig
  { aadhaarImageResizeConfig :: Kernel.Prelude.Maybe Domain.Types.TransporterConfig.AadhaarImageResizeConfig,
    aadhaarVerificationRequired :: Kernel.Prelude.Bool,
    acStatusCheckGap :: Kernel.Prelude.Int,
    actualRideDistanceDiffThreshold :: Kernel.Types.Common.HighPrecMeters,
    actualRideDistanceDiffThresholdIfWithinPickupDrop :: Kernel.Types.Common.HighPrecMeters,
    allowDefaultPlanAllocation :: Kernel.Prelude.Bool,
    approxRideDistanceDiffThreshold :: Kernel.Types.Common.HighPrecMeters,
    arrivalTimeBufferOfVehicle :: Kernel.Prelude.Maybe Domain.Types.TransporterConfig.ArrivalTimeBufferOfVehicle,
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
    cancellationRateCalculationThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    cancellationRateThresholdDaily :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    cancellationRateThresholdWeekly :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    cancellationRateWindow :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
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
    dailyConditionCooldownTimeHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    dailyMinRidesForBlocking :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    dailyMinRidesForNudging :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    dailyOffenceSuspensionTimeHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    defaultPopupDelay :: Kernel.Types.Common.Seconds,
    demandHotspotsConfig :: Kernel.Prelude.Maybe Domain.Types.TransporterConfig.DemandHotspotsConfig,
    disableListScheduledBookingAPI :: Kernel.Prelude.Bool,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    dlNumberVerification :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    dpBlackListedGeohash :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    dpGeoHashPercision :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    dpWhiteListedGeohash :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    driverAutoPayExecutionTime :: Kernel.Prelude.NominalDiffTime,
    driverAutoPayExecutionTimeFallBack :: Kernel.Prelude.NominalDiffTime,
    driverAutoPayNotificationTime :: Kernel.Prelude.NominalDiffTime,
    driverDistanceToPickupThresholdOnCancel :: Kernel.Types.Common.Meters,
    driverDistanceTravelledOnPickupThresholdOnCancel :: Kernel.Types.Common.Meters,
    driverDrivenSearchReqExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.NominalDiffTime,
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
    fleetAlertThreshold :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    freeTrialDays :: Kernel.Prelude.Int,
    graceTimeForScheduledRidePickup :: Kernel.Prelude.NominalDiffTime,
    includeDriverCurrentlyOnRide :: Kernel.Prelude.Bool,
    isAvoidToll :: Kernel.Prelude.Bool,
    isDeviceIdChecksRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isPlanMandatory :: Kernel.Prelude.Bool,
    issueBreachConfig :: Kernel.Prelude.Maybe [SharedLogic.BehaviourManagement.IssueBreach.IssueBreachConfig],
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
    minDistanceForStopFcm :: Kernel.Types.Common.HighPrecMeters,
    minLocationAccuracy :: Kernel.Prelude.Double,
    minRidesForCancellationScore :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minRidesToUnlist :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minThresholdForPassThroughDestination :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
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
    otpRideStartRestrictionRadius :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    overlayBatchSize :: Kernel.Prelude.Int,
    pastDaysRideCounter :: Kernel.Prelude.Int,
    payoutBatchLimit :: Kernel.Prelude.Int,
    pickupLocThreshold :: Kernel.Types.Common.Meters,
    placeNameCacheExpiryDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    popupDelayToAddAsPenalty :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    ratingAsDecimal :: Kernel.Prelude.Bool,
    rcLimit :: Kernel.Prelude.Int,
    recentScheduledBookingsSafeLimit :: Kernel.Prelude.Int,
    recomputeDistanceThresholds :: Kernel.Prelude.Maybe [Domain.Types.TransporterConfig.DistanceRecomputeConfigs],
    recomputeIfPickupDropNotOutsideOfThreshold :: Kernel.Prelude.Bool,
    referralLinkPassword :: Kernel.Prelude.Text,
    refillVehicleModel :: Kernel.Prelude.Bool,
    rideTimeEstimatedThreshold :: Kernel.Types.Common.Seconds,
    routeDeviationThreshold :: Kernel.Types.Common.Meters,
    schedulePayoutForDay :: Kernel.Prelude.Maybe Kernel.Prelude.Integer,
    scheduleRideBufferTime :: Kernel.Prelude.NominalDiffTime,
    scheduledRideFilterExclusionThresholdHours :: Kernel.Types.Common.Hours,
    scheduledRideJobRescheduleTime :: Kernel.Prelude.NominalDiffTime,
    scheduledRideSearchRepeatLimit :: Kernel.Prelude.Int,
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
    upwardsRecomputeBufferPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    useOfferListCache :: Kernel.Prelude.Bool,
    useSilentFCMForForwardBatch :: Kernel.Prelude.Bool,
    useWithSnapToRoadFallback :: Kernel.Prelude.Bool,
    variantsToEnableForSubscription :: [Domain.Types.VehicleVariant.VehicleVariant],
    volunteerSmsSendingLimit :: Kernel.Prelude.Maybe Domain.Types.TransporterConfig.DashboardMediaSendingLimit,
    weeklyConditionCooldownTimeHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    weeklyMinRidesForBlocking :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    weeklyMinRidesForNudging :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    weeklyOffenceSuspensionTimeHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving (Generic, Show, Eq)

data AadhaarImageResizeConfig = AadhaarImageResizeConfig {height :: Kernel.Prelude.Int, width :: Kernel.Prelude.Int} deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data ArrivalTimeBufferOfVehicle = ArrivalTimeBufferOfVehicle
  { ambulance :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    autorickshaw :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    bike :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    black :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    blackxl :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    busAc :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    busNonAc :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    deliveryLightGoodsVehicle :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    deliverybike :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    evautorickshaw :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    hatchback :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    heritagecab :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    premiumsedan :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    sedan :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    suv :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    suvplus :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    taxi :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    taxiplus :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds
  }
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data AvgSpeedOfVechilePerKm = AvgSpeedOfVechilePerKm
  { ambulance :: Kernel.Types.Common.Kilometers,
    autorickshaw :: Kernel.Types.Common.Kilometers,
    bike :: Kernel.Types.Common.Kilometers,
    black :: Kernel.Types.Common.Kilometers,
    blackxl :: Kernel.Types.Common.Kilometers,
    busAc :: Kernel.Types.Common.Kilometers,
    busNonAc :: Kernel.Types.Common.Kilometers,
    deliveryLightGoodsVehicle :: Kernel.Types.Common.Kilometers,
    evautorickshaw :: Kernel.Types.Common.Kilometers,
    hatchback :: Kernel.Types.Common.Kilometers,
    heritagecab :: Kernel.Types.Common.Kilometers,
    premiumsedan :: Kernel.Types.Common.Kilometers,
    sedan :: Kernel.Types.Common.Kilometers,
    suv :: Kernel.Types.Common.Kilometers,
    suvplus :: Kernel.Types.Common.Kilometers,
    taxi :: Kernel.Types.Common.Kilometers,
    taxiplus :: Kernel.Types.Common.Kilometers
  }
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data CancellationRateBasedNudgingAndBlockingConfig = CancellationRateBasedNudgingAndBlockingConfig
  { cancellationRateThresholdDaily :: Kernel.Prelude.Int,
    cancellationRateThresholdWeekly :: Kernel.Prelude.Int,
    dailyConditionCooldownTimeHours :: Kernel.Prelude.Int,
    dailyMinRidesforBlocking :: Kernel.Prelude.Int,
    dailyMinRidesforNudging :: Kernel.Prelude.Int,
    dailyOffenceSuspensionTimeHours :: Kernel.Prelude.Int,
    weeklyConditionCooldownTimeHours :: Kernel.Prelude.Int,
    weeklyMinRidesforBlocking :: Kernel.Prelude.Int,
    weeklyMinRidesforNudging :: Kernel.Prelude.Int,
    weeklyOffenceSuspensionTimeHours :: Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data DashboardMediaSendingLimit = DashboardMediaSendingLimit {alert :: Kernel.Prelude.Int, overlay :: Kernel.Prelude.Int, sms :: Kernel.Prelude.Int, whatsapp :: Kernel.Prelude.Int}
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data DemandHotspotsConfig = DemandHotspotsConfig
  { analysisDurationMinutes :: Kernel.Prelude.Int,
    enableDemandHotspots :: Kernel.Prelude.Bool,
    noOfGeohashesToReturn :: Kernel.Prelude.Int,
    precisionOfGeohash :: Kernel.Prelude.Int,
    resultDurationMinutes :: Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data DistanceRecomputeConfigs = DistanceRecomputeConfigs {estimatedDistanceUpper :: Kernel.Types.Common.Meters, minThresholdDistance :: Kernel.Types.Common.Meters, minThresholdPercentage :: Kernel.Prelude.Int}
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

type TransporterConfig = TransporterConfigD 'Safe

instance FromJSON (TransporterConfigD 'Unsafe)

instance ToJSON (TransporterConfigD 'Unsafe)

instance FromJSON (TransporterConfigD 'Safe)

instance ToJSON (TransporterConfigD 'Safe)
