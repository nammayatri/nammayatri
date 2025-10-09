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
import qualified Domain.Types.Person
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
  { aaEnabledClientSdkVersion :: Kernel.Prelude.Text,
    aadhaarImageResizeConfig :: Kernel.Prelude.Maybe Domain.Types.TransporterConfig.AadhaarImageResizeConfig,
    aadhaarVerificationRequired :: Kernel.Prelude.Bool,
    acStatusCheckGap :: Kernel.Prelude.Int,
    actualRideDistanceDiffThreshold :: Kernel.Types.Common.HighPrecMeters,
    actualRideDistanceDiffThresholdIfWithinPickupDrop :: Kernel.Types.Common.HighPrecMeters,
    allowDefaultPlanAllocation :: Kernel.Prelude.Bool,
    allowDriverToUseFleetRcs :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    allowDuplicateAadhaar :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    allowDuplicateGst :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    allowDuplicatePan :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    allowedReferralEntities :: [Domain.Types.Person.Role],
    analyticsConfig :: Domain.Types.TransporterConfig.AnalyticsConfig,
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
    bulkWaiveOffLimit :: Kernel.Prelude.Int,
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
    cancellationFeeCycle :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    cancellationFeeDisputeLimit :: Kernel.Prelude.Int,
    cancellationFeeDisputeWindow :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    cancellationFeeVendor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cancellationRateCalculationThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    cancellationRateSlabConfig :: Kernel.Prelude.Maybe Domain.Types.TransporterConfig.CancellationRateSlabConfig,
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
    deactivateRCOnUnlink :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    defaultPopupDelay :: Kernel.Types.Common.Seconds,
    demandHotspotsConfig :: Kernel.Prelude.Maybe Domain.Types.TransporterConfig.DemandHotspotsConfig,
    disableDriverWhenUnlinkingVehicle :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    disableListScheduledBookingAPI :: Kernel.Prelude.Bool,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    distanceWeightage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    dlNumberVerification :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    dpBlackListedGeohash :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    dpGeoHashPercision :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    dpWhiteListedGeohash :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    driverAutoPayExecutionTime :: Kernel.Prelude.NominalDiffTime,
    driverAutoPayExecutionTimeFallBack :: Kernel.Prelude.NominalDiffTime,
    driverAutoPayNotificationTime :: Kernel.Prelude.NominalDiffTime,
    driverCallingOption :: Kernel.Prelude.Maybe Domain.Types.TransporterConfig.CallingOption,
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
    driverWalletConfig :: Domain.Types.TransporterConfig.DriverWalletConfig,
    dropLocThreshold :: Kernel.Types.Common.Meters,
    dummyFromLocation :: Domain.Types.Location.DummyLocationInfo,
    dummyShowDriverAdditions :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    dummyToLocation :: Domain.Types.Location.DummyLocationInfo,
    dynamicReferralCodeEnabled :: Kernel.Prelude.Bool,
    dynamicReferralCodeValidForMinutes :: Kernel.Prelude.Integer,
    editLocDriverPermissionNeeded :: Kernel.Prelude.Bool,
    editLocTimeThreshold :: Kernel.Types.Common.Seconds,
    emailOtpConfig :: Kernel.Prelude.Maybe Email.Types.EmailOTPConfig,
    enableDashboardSms :: Kernel.Prelude.Bool,
    enableExistingVehicleInBulkUpload :: Kernel.Prelude.Bool,
    enableFaceVerification :: Kernel.Prelude.Bool,
    enableFareCalculatorV2 :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    enableOverchargingBlocker :: Kernel.Prelude.Bool,
    enableTollCrossedNotifications :: Kernel.Prelude.Bool,
    enableUdfForOffers :: Kernel.Prelude.Bool,
    enableVendorCheckForCollectingDues :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    exotelAppIdMapping :: Kernel.Prelude.Maybe Domain.Types.Extra.TransporterConfig.ExotelMapping,
    exotelStatusCheckSchedulerDelay :: Kernel.Prelude.Int,
    fakeOtpEmails :: [Kernel.Prelude.Text],
    fakeOtpMobileNumbers :: [Kernel.Prelude.Text],
    fareRecomputeDailyExtraKmsThreshold :: Kernel.Types.Common.HighPrecMeters,
    fareRecomputeWeeklyExtraKmsThreshold :: Kernel.Types.Common.HighPrecMeters,
    fcmConfig :: Kernel.External.Notification.FCM.Types.FCMConfig,
    fleetAlertThreshold :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    freeTrialDays :: Kernel.Prelude.Int,
    generateReferralCodeForFleet :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    generateReferralCodeForOperator :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    graceTimeForScheduledRidePickup :: Kernel.Prelude.NominalDiffTime,
    includeDriverCurrentlyOnRide :: Kernel.Prelude.Bool,
    isAAEnabledForRecurring :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isAvoidToll :: Kernel.Prelude.Bool,
    isDeviceIdChecksRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDynamicPricingQARCalEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isMLBasedDynamicPricingEnabled :: Kernel.Prelude.Bool,
    isPlanMandatory :: Kernel.Prelude.Bool,
    issueBreachConfig :: Kernel.Prelude.Maybe [SharedLogic.BehaviourManagement.IssueBreach.IssueBreachConfig],
    kaptureDisposition :: Kernel.Prelude.Text,
    kaptureQueue :: Kernel.Prelude.Text,
    languagesToBeTranslated :: [Kernel.External.Types.Language],
    lastNdaysToCheckForPayoutOrderStatus :: Kernel.Prelude.Int,
    liveEKD :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    localAmbulanceNumbers :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    localPoliceNumbers :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    mandateExecutionRescheduleInterval :: Kernel.Prelude.NominalDiffTime,
    mandateNotificationRescheduleInterval :: Kernel.Prelude.NominalDiffTime,
    mandateValidity :: Kernel.Prelude.Int,
    maxAllowedDocSizeInMB :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxAllowedVideoDocSizeInMB :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxPayoutReferralForADay :: Kernel.Prelude.Int,
    mediaFileSizeUpperLimit :: Kernel.Prelude.Int,
    mediaFileUrlPattern :: Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    meterRideBulkLocUpdateBatchSize :: Kernel.Prelude.Integer,
    minDistanceForStopFcm :: Kernel.Types.Common.HighPrecMeters,
    minLocationAccuracy :: Kernel.Prelude.Double,
    minRidesForCancellationScore :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minRidesToUnlist :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minThresholdForPassThroughDestination :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    minmRentalAndScheduledBookingLeadTimeHours :: Kernel.Types.Common.Hours,
    nightSafetyEndTime :: Kernel.Types.Common.Seconds,
    nightSafetyRouteDeviationThreshold :: Kernel.Types.Common.Meters,
    nightSafetyStartTime :: Kernel.Types.Common.Seconds,
    normalRideBulkLocUpdateBatchSize :: Kernel.Prelude.Integer,
    notificationRetryCountThreshold :: Kernel.Prelude.Int,
    notificationRetryEligibleErrorCodes :: [Kernel.Prelude.Text],
    notificationRetryTimeGap :: Kernel.Prelude.NominalDiffTime,
    numOfCancellationsAllowed :: Kernel.Prelude.Int,
    onboardingDocsCountLimit :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
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
    qarCalRadiusInKm :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    ratingAsDecimal :: Kernel.Prelude.Bool,
    rcChangeThresholdDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rcExpiryChecks :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    rcLimit :: Kernel.Prelude.Int,
    recentScheduledBookingsSafeLimit :: Kernel.Prelude.Int,
    recomputeDistanceThresholds :: Kernel.Prelude.Maybe [Domain.Types.TransporterConfig.DistanceRecomputeConfigs],
    recomputeIfPickupDropNotOutsideOfThreshold :: Kernel.Prelude.Bool,
    referralLinkPassword :: Kernel.Prelude.Text,
    refillVehicleModel :: Kernel.Prelude.Bool,
    requireRouteMappingInVehicle :: Kernel.Prelude.Bool,
    requiresOnboardingInspection :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    rideTimeEstimatedThreshold :: Kernel.Types.Common.Seconds,
    routeDeviationThreshold :: Kernel.Types.Common.Meters,
    safetyTeamNumbers :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
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
    subscriptionConfig :: Domain.Types.TransporterConfig.SubscriptionConfig,
    subscriptionStartTime :: Kernel.Prelude.UTCTime,
    thresholdCancellationPercentageToUnlist :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    thresholdCancellationScore :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    timeDiffFromUtc :: Kernel.Types.Common.Seconds,
    toNotifyDriverForExtraKmsLimitExceed :: Kernel.Prelude.Bool,
    updateNotificationStatusBatchSize :: Kernel.Prelude.Int,
    updateOrderStatusBatchSize :: Kernel.Prelude.Int,
    updatePayoutStatusBatchSize :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    upgradeTierDropRetentionTime :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    upwardsRecomputeBuffer :: Kernel.Types.Common.HighPrecMeters,
    upwardsRecomputeBufferPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    useOfferListCache :: Kernel.Prelude.Bool,
    useSilentFCMForForwardBatch :: Kernel.Prelude.Bool,
    useWithSnapToRoadFallback :: Kernel.Prelude.Bool,
    validNameComparePercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    variantsToEnableForSubscription :: [Domain.Types.VehicleVariant.VehicleVariant],
    volunteerSmsSendingLimit :: Kernel.Prelude.Maybe Domain.Types.TransporterConfig.DashboardMediaSendingLimit,
    weeklyConditionCooldownTimeHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    weeklyMinRidesForBlocking :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    weeklyMinRidesForNudging :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    weeklyOffenceSuspensionTimeHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving (Generic, Show, Eq)

data AadhaarImageResizeConfig = AadhaarImageResizeConfig {height :: Kernel.Prelude.Int, width :: Kernel.Prelude.Int} deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data AnalyticsConfig = AnalyticsConfig
  { allowCacheDriverFlowStatus :: Kernel.Prelude.Bool,
    earningsWindowSize :: Kernel.Prelude.Int,
    enableFleetOperatorDashboardAnalytics :: Kernel.Prelude.Bool,
    maxOnlineDurationDays :: Kernel.Prelude.Int,
    onlineDurationCalculateFrom :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    weekStartMode :: Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data ArrivalTimeBufferOfVehicle = ArrivalTimeBufferOfVehicle
  { ambulance :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    autorickshaw :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    bike :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    bikeplus :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    black :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    blackxl :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    boat :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    busAc :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    busNonAc :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    deliveryLightGoodsVehicle :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    deliverybike :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    erickshaw :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    evautorickshaw :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    hatchback :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    heritagecab :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    premiumsedan :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    sedan :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    suv :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    suvplus :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    taxi :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    taxiplus :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    vipEscort :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    vipOfficer :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds
  }
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data AvgSpeedOfVechilePerKm = AvgSpeedOfVechilePerKm
  { ambulance :: Kernel.Types.Common.Kilometers,
    autorickshaw :: Kernel.Types.Common.Kilometers,
    bike :: Kernel.Types.Common.Kilometers,
    bikeplus :: Kernel.Types.Common.Kilometers,
    black :: Kernel.Types.Common.Kilometers,
    blackxl :: Kernel.Types.Common.Kilometers,
    boat :: Kernel.Types.Common.Kilometers,
    busAc :: Kernel.Types.Common.Kilometers,
    busNonAc :: Kernel.Types.Common.Kilometers,
    deliveryLightGoodsVehicle :: Kernel.Types.Common.Kilometers,
    erickshaw :: Kernel.Types.Common.Kilometers,
    evautorickshaw :: Kernel.Types.Common.Kilometers,
    hatchback :: Kernel.Types.Common.Kilometers,
    heritagecab :: Kernel.Types.Common.Kilometers,
    premiumsedan :: Kernel.Types.Common.Kilometers,
    sedan :: Kernel.Types.Common.Kilometers,
    suv :: Kernel.Types.Common.Kilometers,
    suvplus :: Kernel.Types.Common.Kilometers,
    taxi :: Kernel.Types.Common.Kilometers,
    taxiplus :: Kernel.Types.Common.Kilometers,
    vipEscort :: Kernel.Types.Common.Kilometers,
    vipOfficer :: Kernel.Types.Common.Kilometers
  }
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data CallingOption = AnonymousCall | DirectCall | DualCall deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data CancellationRateBasedNudgingAndBlockingConfig = CancellationRateBasedNudgingAndBlockingConfig
  { cancellationRateSlabConfig :: Kernel.Prelude.Maybe Domain.Types.TransporterConfig.CancellationRateSlabConfig,
    cancellationRateThresholdDaily :: Kernel.Prelude.Int,
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

data CancellationRateSlab = CancellationRateSlab {cancellationPercentageThreshold :: Kernel.Prelude.Int, suspensionTimeInHours :: Kernel.Prelude.Int}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data CancellationRateSlabConfig = CancellationRateSlabConfig {dailySlabs :: [Domain.Types.TransporterConfig.SlabType], weeklySlabs :: [Domain.Types.TransporterConfig.SlabType]}
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

data DriverWalletConfig = DriverWalletConfig
  { driverWalletPayoutThreshold :: Kernel.Types.Common.HighPrecMoney,
    enableDriverWallet :: Kernel.Prelude.Bool,
    enableWalletPayout :: Kernel.Prelude.Bool,
    enableWalletTopup :: Kernel.Prelude.Bool,
    gstPercentage :: Kernel.Prelude.Double,
    maxWalletPayoutsPerDay :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minWalletAmountForCashRides :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    minimumWalletPayoutAmount :: Kernel.Types.Common.HighPrecMoney,
    payoutCutOffDays :: Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data SlabType = SlabType {minBookingsRange :: [Kernel.Prelude.Int], penalityForCancellation :: Domain.Types.TransporterConfig.CancellationRateSlab}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data SubscriptionConfig = SubscriptionConfig
  { fleetPrepaidSubscriptionThreshold :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    prepaidSubscriptionThreshold :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

type TransporterConfig = TransporterConfigD 'Safe

instance FromJSON (TransporterConfigD 'Unsafe)

instance ToJSON (TransporterConfigD 'Unsafe)

instance FromJSON (TransporterConfigD 'Safe)

instance ToJSON (TransporterConfigD 'Safe)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CallingOption)
