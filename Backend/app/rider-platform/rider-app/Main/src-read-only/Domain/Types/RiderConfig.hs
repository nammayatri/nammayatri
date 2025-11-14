{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RiderConfig (module Domain.Types.RiderConfig, module ReExport) where

import qualified BecknV2.FRFS.Enums
import qualified BecknV2.OnDemand.Enums
import Data.Aeson
import qualified Data.Time
import Domain.Types.Extra.RiderConfig as ReExport
import qualified Domain.Types.Extra.RiderConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MultimodalPreferences
import qualified Domain.Types.RentalsIntercityCache
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.VehicleVariant
import qualified Email.Types
import qualified IssueManagement.Common
import qualified Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RiderConfig = RiderConfig
  { appUrl :: Kernel.Prelude.Text,
    autoSendBookingDetailsViaWhatsapp :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    autoUnblockSafetyCenterAfterDays :: Kernel.Prelude.Int,
    avgSpeedInKmPerHr :: Kernel.Types.Common.Kilometers,
    blockedUntilInMins :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    bookingSyncStatusCallSecondsDiffThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    boostSearchPreSelectionServiceTierConfig :: [Domain.Types.Extra.RiderConfig.VehicleServiceTierOrderConfig],
    busBookingAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    busScanRouteCalculationEnabledModes :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    busTierSortingConfig :: Kernel.Prelude.Maybe [Domain.Types.RiderConfig.BusTierSortingConfig],
    busTrackingConfig :: Kernel.Prelude.Maybe Domain.Types.RiderConfig.BusTrackingConfig,
    busTransitTypes :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType],
    cancellationPaymentDelay :: Kernel.Prelude.NominalDiffTime,
    collectAutoCompleteData :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    collectMMIRouteData :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    csAlertTriggerDelay :: Kernel.Prelude.NominalDiffTime,
    cxAgentDetails :: Kernel.Prelude.Maybe [IssueManagement.Common.CxAgentDetails],
    dashboardMediaFileUrlPattern :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    distanceToNearestStopThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    distanceWeightage :: Kernel.Prelude.Int,
    domainPublicTransportDataVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    domainRouteCalculationEnabledModes :: Kernel.Prelude.Maybe [Kernel.External.MultiModal.Interface.Types.GeneralVehicleType],
    driverReferredSearchReqExpiry :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    dynamicLogicUpdatePassword :: Kernel.Prelude.Text,
    emailBusinessVerificationConfig :: Kernel.Prelude.Maybe Email.Types.EmailBusinessVerificationConfig,
    emailMagicLinkConfig :: Kernel.Prelude.Maybe Email.Types.EmailMagicLinkConfig,
    emailOtpConfig :: Kernel.Prelude.Maybe Email.Types.EmailOTPConfig,
    enableAutoJourneyRefund :: Kernel.Prelude.Bool,
    enableBusFiltering :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    enableEmergencyContactAddedMessage :: Kernel.Prelude.Bool,
    enableIGMIssueFlow :: Kernel.Prelude.Bool,
    enableLocalPoliceSupport :: Kernel.Prelude.Bool,
    enableMultiModalForAllUsers :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    enableOnlinePaymentRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    enableRideEndOffers :: Kernel.Prelude.Bool,
    enableSupportForSafety :: Kernel.Prelude.Bool,
    excludedVehicleVariants :: Kernel.Prelude.Maybe [Domain.Types.ServiceTierType.ServiceTierType],
    executePaymentDelay :: Kernel.Prelude.NominalDiffTime,
    exotelAppIdMapping :: Kernel.Prelude.Maybe Domain.Types.Extra.RiderConfig.ExotelMapping,
    exotelStatusCheckSchedulerDelay :: Kernel.Prelude.Int,
    fareCacheInterCitySearchLocations :: Kernel.Prelude.Maybe [Domain.Types.RentalsIntercityCache.IntercitySearchLocation],
    fareCacheRentalsConfig :: Kernel.Prelude.Maybe [Domain.Types.RentalsIntercityCache.RentalsConfig],
    feedbackAlertRatingThreshold :: Kernel.Prelude.Int,
    filterWalkAndUnspecifiedTransitModes :: Kernel.Prelude.Bool,
    frfsMetricsApiKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    hardLimitForSafetyJobs :: Kernel.Prelude.Int,
    incidentReportSupport :: Kernel.Prelude.Bool,
    initiateFirstMultimodalJourney :: Kernel.Prelude.Bool,
    intercitySearchLocations :: Kernel.Prelude.Maybe [Domain.Types.RentalsIntercityCache.IntercitySearchLocation],
    isAvoidToll :: Kernel.Prelude.Bool,
    isDeviceIdCheckDisabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isFirstReferredRideEnabled :: Kernel.Prelude.Bool,
    ivrTriggerDelay :: Kernel.Prelude.NominalDiffTime,
    journeyOptionsSortingType :: Kernel.Prelude.Maybe Domain.Types.MultimodalPreferences.JourneyOptionsSortingType,
    kaptureConfig :: IssueManagement.Common.KaptureConfig,
    kaptureQueue :: Kernel.Prelude.Text,
    localPoliceNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    makeMultiModalSearch :: Kernel.Prelude.Bool,
    maxAllowedPublicTransportLegs :: Kernel.Prelude.Int,
    maxNearbyBuses :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maximumWalkDistance :: Kernel.Types.Common.Meters,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    metricsBlacklistPatterns :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    metroBookingAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    minRidesToBlock :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minRidesToShowCancellationRate :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minimumWalkDistance :: Kernel.Types.Common.Meters,
    multimodalTesting :: Kernel.Prelude.Bool,
    nearByDriverRingBucketCfg :: Kernel.Prelude.Maybe [Domain.Types.RiderConfig.RingBucketCfg],
    nearbyBusMaxTimeThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    nearbyBusSearchRadius :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    nearbyDriverSearchRadius :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    noOfRideRequestsConfig :: Kernel.Prelude.Int,
    nyRegularExecutionTimeOffsetMinutes :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    nyRegularMasterJobNextRunOffsetSeconds :: Kernel.Prelude.NominalDiffTime,
    nyRegularMinGapSeconds :: Kernel.Types.Common.Seconds,
    nyRegularSubscriptionBatchSize :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    offerListCacheVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutBatchDelay :: Kernel.Prelude.NominalDiffTime,
    payoutBatchSize :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    payoutReferralProgram :: Kernel.Prelude.Bool,
    payoutReferralStartDate :: Kernel.Prelude.UTCTime,
    payoutReferralThresholdPerDay :: Kernel.Prelude.Int,
    payoutReferralThresholdPerMonth :: Kernel.Prelude.Int,
    permissibleModes :: Kernel.Prelude.Maybe [Kernel.External.MultiModal.Interface.Types.GeneralVehicleType],
    pickupInstructionsProximityMeters :: Kernel.Prelude.Int,
    pickupInstructionsThreshold :: Kernel.Prelude.Int,
    placeNameCacheExpiryDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    policeTriggerDelay :: Kernel.Prelude.NominalDiffTime,
    postRideSafetyNotificationDelay :: Kernel.Prelude.NominalDiffTime,
    qrTicketRestrictionEndTime :: Kernel.Prelude.Maybe Data.Time.TimeOfDay,
    qrTicketRestrictionStartTime :: Kernel.Prelude.Maybe Data.Time.TimeOfDay,
    refundBufferTTLSec :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    refundStatusUpdateInterval :: Kernel.Prelude.NominalDiffTime,
    refundStatusUpdateRetries :: Kernel.Prelude.Int,
    rentalsConfig :: Kernel.Prelude.Maybe [Domain.Types.RentalsIntercityCache.RentalsConfig],
    safetyCheckEndTime :: Kernel.Types.Common.Seconds,
    safetyCheckStartTime :: Kernel.Types.Common.Seconds,
    sensitiveWords :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    sensitiveWordsForExactMatch :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    serviceTierRelationshipCfg :: Kernel.Prelude.Maybe [Domain.Types.RiderConfig.ServiceTierRelationshipCfg],
    settleCancellationFeeBeforeNextRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    sourceOfServiceTier :: Domain.Types.RiderConfig.ServiceTierSource,
    specialVehicleNotificationConfigs :: Kernel.Prelude.Maybe [Domain.Types.RiderConfig.SpecialVehicleNotificationConfig],
    specialZoneRadius :: Kernel.Prelude.Int,
    suburbanBookingAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    subwayRestrictionEndTime :: Kernel.Prelude.Maybe Data.Time.TimeOfDay,
    subwayRestrictionStartTime :: Kernel.Prelude.Maybe Data.Time.TimeOfDay,
    subwayTransitTypes :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType],
    thresholdCancellationPercentageToBlock :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    ticketAssetDomain :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ticketingPermissionConfig :: Kernel.Prelude.Maybe Data.Aeson.Value,
    timeDiffFromUtc :: Kernel.Types.Common.Seconds,
    trackVehicleKeyExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    trackingShortUrlPattern :: Kernel.Prelude.Text,
    upcomingBusThresholdSec :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    updateTicketValidityInSecondsPostSetOnboarding :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    useUserSettingsForSafetyIVR :: Kernel.Prelude.Bool,
    userServiceTierOrderConfig :: [Domain.Types.Extra.RiderConfig.VehicleServiceTierOrderConfig],
    validateSetOnboardingVehicleRequest :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    variantListForNearByReq :: Kernel.Prelude.Maybe [Domain.Types.VehicleVariant.VehicleVariant],
    videoFileSizeUpperLimit :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data BusTierSortingConfig = BusTierSortingConfig {rank :: Kernel.Prelude.Int, tier :: BecknV2.FRFS.Enums.ServiceTierType} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data BusTrackingConfig = BusTrackingConfig
  { fairScore :: Kernel.Prelude.Double,
    fairScoreDistanceInMeters :: Kernel.Prelude.Double,
    goodScore :: Kernel.Prelude.Double,
    goodScoreDistanceInMeters :: Kernel.Prelude.Double,
    maxScore :: Kernel.Prelude.Double,
    maxScoreDistanceInMeters :: Kernel.Prelude.Double,
    movementThresholdInMeters :: Kernel.Prelude.Double,
    thresholdFactor :: Kernel.Prelude.Double,
    thresholdSeconds :: Kernel.Prelude.Double
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data RingBucketCfg = RingBucketCfg {radiusInMeters :: Kernel.Types.Common.Meters, size :: Kernel.Prelude.Int, vehVariant :: Domain.Types.VehicleVariant.VehicleVariant}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data ServiceTierRelationshipCfg = ServiceTierRelationshipCfg {canBoardIn :: [BecknV2.FRFS.Enums.ServiceTierType], serviceTierType :: BecknV2.FRFS.Enums.ServiceTierType, vehicleType :: BecknV2.OnDemand.Enums.VehicleCategory}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data ServiceTierSource = NANDI | QUOTES deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SpecialVehicleNotificationConfig = SpecialVehicleNotificationConfig {notificationMessage :: Kernel.Prelude.Text, notificationTitle :: Kernel.Prelude.Text, vehicleNo :: Kernel.Prelude.Text}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ServiceTierSource)
