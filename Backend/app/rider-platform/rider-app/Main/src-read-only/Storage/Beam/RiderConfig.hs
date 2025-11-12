{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RiderConfig where

import qualified BecknV2.FRFS.Enums
import qualified Data.Aeson
import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.RiderConfig
import qualified Domain.Types.MultimodalPreferences
import qualified Domain.Types.RentalsIntercityCache
import qualified Domain.Types.RiderConfig
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.UtilsTH
import qualified Email.Types
import qualified IssueManagement.Common
import Kernel.External.Encryption
import qualified Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data RiderConfigT f = RiderConfigT
  { appUrl :: B.C f Kernel.Prelude.Text,
    autoSendBookingDetailsViaWhatsapp :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    autoUnblockSafetyCenterAfterDays :: B.C f Kernel.Prelude.Int,
    avgSpeedInKmPerHr :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Kilometers),
    blockedUntilInMins :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Minutes),
    bookingSyncStatusCallSecondsDiffThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    boostSearchPreSelectionServiceTierConfig :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    busBookingAllowed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    busScanRouteCalculationEnabledModes :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    busTierSortingConfig :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    busTrackingConfig :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    busTransitTypes :: B.C f (Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType]),
    cancellationPaymentDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    collectAutoCompleteData :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    collectMMIRouteData :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    csAlertTriggerDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    cxAgentDetails :: B.C f (Kernel.Prelude.Maybe [IssueManagement.Common.CxAgentDetails]),
    dashboardMediaFileUrlPattern :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    distanceToNearestStopThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    distanceWeightage :: B.C f Kernel.Prelude.Int,
    domainPublicTransportDataVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    domainRouteCalculationEnabledModes :: B.C f (Kernel.Prelude.Maybe [Kernel.External.MultiModal.Interface.Types.GeneralVehicleType]),
    driverReferredSearchReqExpiry :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    dynamicLogicUpdatePassword :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    emailBusinessVerificationConfig :: B.C f (Kernel.Prelude.Maybe Email.Types.EmailBusinessVerificationConfig),
    emailMagicLinkConfig :: B.C f (Kernel.Prelude.Maybe Email.Types.EmailMagicLinkConfig),
    emailOtpConfig :: B.C f (Kernel.Prelude.Maybe Email.Types.EmailOTPConfig),
    enableAutoJourneyRefund :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    enableBusFiltering :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    enableEmergencyContactAddedMessage :: B.C f Kernel.Prelude.Bool,
    enableIGMIssueFlow :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    enableLocalPoliceSupport :: B.C f Kernel.Prelude.Bool,
    enableMultiModalForAllUsers :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    enableOnlinePaymentRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    enableRideEndOffers :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    enableSupportForSafety :: B.C f Kernel.Prelude.Bool,
    excludedVehicleVariants :: B.C f (Kernel.Prelude.Maybe [Domain.Types.ServiceTierType.ServiceTierType]),
    executePaymentDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    exotelAppIdMapping :: B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.RiderConfig.ExotelMapping),
    exotelStatusCheckSchedulerDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    fareCacheInterCitySearchLocations :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    fareCacheRentalsConfig :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    feedbackAlertRatingThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    filterWalkAndUnspecifiedTransitModes :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    hardLimitForSafetyJobs :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    incidentReportSupport :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    initiateFirstMultimodalJourney :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    intercitySearchLocations :: B.C f (Kernel.Prelude.Maybe [Domain.Types.RentalsIntercityCache.IntercitySearchLocation]),
    isAvoidToll :: B.C f Kernel.Prelude.Bool,
    isDeviceIdCheckDisabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isFirstReferredRideEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    ivrTriggerDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    journeyOptionsSortingType :: B.C f (Kernel.Prelude.Maybe Domain.Types.MultimodalPreferences.JourneyOptionsSortingType),
    kaptureConfig :: B.C f IssueManagement.Common.KaptureConfig,
    kaptureQueue :: B.C f Kernel.Prelude.Text,
    localPoliceNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    makeMultiModalSearch :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    maxAllowedPublicTransportLegs :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    maxNearbyBuses :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    maximumWalkDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    metricsBlacklistPatterns :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    metroBookingAllowed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    minRidesToBlock :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    minRidesToShowCancellationRate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    minimumWalkDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    multimodalTesting :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    nearByDriverRingBucketCfg :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    nearbyBusMaxTimeThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    nearbyBusSearchRadius :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    nearbyDriverSearchRadius :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    noOfRideRequestsConfig :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    nyRegularExecutionTimeOffsetMinutes :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    nyRegularMasterJobNextRunOffsetSeconds :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    nyRegularMinGapSeconds :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    nyRegularSubscriptionBatchSize :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    offerListCacheVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    payoutBatchDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    payoutBatchSize :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    payoutReferralProgram :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    payoutReferralStartDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    payoutReferralThresholdPerDay :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    payoutReferralThresholdPerMonth :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    permissibleModes :: B.C f (Kernel.Prelude.Maybe [Kernel.External.MultiModal.Interface.Types.GeneralVehicleType]),
    pickupInstructionsProximityMeters :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    pickupInstructionsThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    placeNameCacheExpiryDays :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    policeTriggerDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    postRideSafetyNotificationDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    qrTicketRestrictionEndTime :: B.C f (Kernel.Prelude.Maybe Data.Time.TimeOfDay),
    qrTicketRestrictionStartTime :: B.C f (Kernel.Prelude.Maybe Data.Time.TimeOfDay),
    refundBufferTTLSec :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    refundStatusUpdateInterval :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    refundStatusUpdateRetries :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    rentalsConfig :: B.C f (Kernel.Prelude.Maybe [Domain.Types.RentalsIntercityCache.RentalsConfig]),
    safetyCheckEndTime :: B.C f Kernel.Types.Common.Seconds,
    safetyCheckStartTime :: B.C f Kernel.Types.Common.Seconds,
    sensitiveWords :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    sensitiveWordsForExactMatch :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    serviceTierRelationshipCfg :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    settleCancellationFeeBeforeNextRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    sourceOfServiceTier :: B.C f (Kernel.Prelude.Maybe Domain.Types.RiderConfig.ServiceTierSource),
    specialVehicleNotificationConfigs :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    specialZoneRadius :: B.C f Kernel.Prelude.Int,
    suburbanBookingAllowed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    subwayRestrictionEndTime :: B.C f (Kernel.Prelude.Maybe Data.Time.TimeOfDay),
    subwayRestrictionStartTime :: B.C f (Kernel.Prelude.Maybe Data.Time.TimeOfDay),
    subwayTransitTypes :: B.C f (Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType]),
    thresholdCancellationPercentageToBlock :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    ticketAssetDomain :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    ticketingPermissionConfig :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    timeDiffFromUtc :: B.C f Kernel.Types.Common.Seconds,
    trackVehicleKeyExpiry :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    trackingShortUrlPattern :: B.C f Kernel.Prelude.Text,
    upcomingBusThresholdSec :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    updateTicketValidityInSecondsPostSetOnboarding :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    useUserSettingsForSafetyIVR :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    userServiceTierOrderConfig :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    validateSetOnboardingVehicleRequest :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    variantListForNearByReq :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    videoFileSizeUpperLimit :: B.C f Kernel.Prelude.Int,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RiderConfigT where
  data PrimaryKey RiderConfigT f = RiderConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RiderConfigId . merchantOperatingCityId

type RiderConfig = RiderConfigT Identity

$(enableKVPG ''RiderConfigT ['merchantOperatingCityId] [])

$(mkTableInstances ''RiderConfigT "rider_config")

$(Domain.Types.UtilsTH.mkCacParseInstance ''RiderConfigT)
