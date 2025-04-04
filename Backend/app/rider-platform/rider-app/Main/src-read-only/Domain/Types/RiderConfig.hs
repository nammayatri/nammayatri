{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RiderConfig (module Domain.Types.RiderConfig, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.RiderConfig as ReExport
import qualified Domain.Types.Extra.RiderConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
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
    cancellationPaymentDelay :: Kernel.Prelude.NominalDiffTime,
    collectAutoCompleteData :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    collectMMIRouteData :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    csAlertTriggerDelay :: Kernel.Prelude.NominalDiffTime,
    cxAgentDetails :: Kernel.Prelude.Maybe [IssueManagement.Common.CxAgentDetails],
    distanceWeightage :: Kernel.Prelude.Int,
    driverReferredSearchReqExpiry :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    dynamicLogicUpdatePassword :: Kernel.Prelude.Text,
    emailOtpConfig :: Kernel.Prelude.Maybe Email.Types.EmailOTPConfig,
    enableEmergencyContactAddedMessage :: Kernel.Prelude.Bool,
    enableLocalPoliceSupport :: Kernel.Prelude.Bool,
    enableSupportForSafety :: Kernel.Prelude.Bool,
    excludedVehicleVariants :: Kernel.Prelude.Maybe [Domain.Types.ServiceTierType.ServiceTierType],
    executePaymentDelay :: Kernel.Prelude.NominalDiffTime,
    exotelAppIdMapping :: Kernel.Prelude.Maybe Domain.Types.Extra.RiderConfig.ExotelMapping,
    exotelStatusCheckSchedulerDelay :: Kernel.Prelude.Int,
    fareCacheInterCitySearchLocations :: Kernel.Prelude.Maybe [Domain.Types.RentalsIntercityCache.IntercitySearchLocation],
    fareCacheRentalsConfig :: Kernel.Prelude.Maybe [Domain.Types.RentalsIntercityCache.RentalsConfig],
    feedbackAlertRatingThreshold :: Kernel.Prelude.Int,
    hardLimitForSafetyJobs :: Kernel.Prelude.Int,
    incidentReportSupport :: Kernel.Prelude.Bool,
    intercitySearchLocations :: Kernel.Prelude.Maybe [Domain.Types.RentalsIntercityCache.IntercitySearchLocation],
    isAvoidToll :: Kernel.Prelude.Bool,
    isDeviceIdCheckDisabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isFirstReferredRideEnabled :: Kernel.Prelude.Bool,
    ivrTriggerDelay :: Kernel.Prelude.NominalDiffTime,
    kaptureConfig :: IssueManagement.Common.KaptureConfig,
    kaptureQueue :: Kernel.Prelude.Text,
    localPoliceNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    makeMultiModalSearch :: Kernel.Prelude.Bool,
    maxAllowedPublicTransportLegs :: Kernel.Prelude.Int,
    maximumWalkDistance :: Kernel.Types.Common.Meters,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    metroBookingAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    minRidesToBlock :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minRidesToShowCancellationRate :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minimumWalkDistance :: Kernel.Types.Common.Meters,
    multimodalTesting :: Kernel.Prelude.Bool,
    nearByDriverRingBucketCfg :: Kernel.Prelude.Maybe [Domain.Types.RiderConfig.RingBucketCfg],
    payoutBatchDelay :: Kernel.Prelude.NominalDiffTime,
    payoutBatchSize :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    payoutReferralProgram :: Kernel.Prelude.Bool,
    payoutReferralStartDate :: Kernel.Prelude.UTCTime,
    payoutReferralThresholdPerDay :: Kernel.Prelude.Int,
    payoutReferralThresholdPerMonth :: Kernel.Prelude.Int,
    permissibleModes :: Kernel.Prelude.Maybe [Kernel.External.MultiModal.Interface.Types.GeneralVehicleType],
    placeNameCacheExpiryDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    policeTriggerDelay :: Kernel.Prelude.NominalDiffTime,
    postRideSafetyNotificationDelay :: Kernel.Prelude.NominalDiffTime,
    rentalsConfig :: Kernel.Prelude.Maybe [Domain.Types.RentalsIntercityCache.RentalsConfig],
    safetyCheckEndTime :: Kernel.Types.Common.Seconds,
    safetyCheckStartTime :: Kernel.Types.Common.Seconds,
    sensitiveWords :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    sensitiveWordsForExactMatch :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    settleCancellationFeeBeforeNextRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    specialZoneRadius :: Kernel.Prelude.Int,
    straightLineThreshold :: Kernel.Types.Common.Meters,
    suburbanBookingAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    thresholdCancellationPercentageToBlock :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    timeDiffFromUtc :: Kernel.Types.Common.Seconds,
    trackingShortUrlPattern :: Kernel.Prelude.Text,
    useUserSettingsForSafetyIVR :: Kernel.Prelude.Bool,
    variantListForNearByReq :: Kernel.Prelude.Maybe [Domain.Types.VehicleVariant.VehicleVariant],
    videoFileSizeUpperLimit :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data RingBucketCfg = RingBucketCfg {radiusInMeters :: Kernel.Types.Common.Meters, size :: Kernel.Prelude.Int, vehVariant :: Domain.Types.VehicleVariant.VehicleVariant}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)
