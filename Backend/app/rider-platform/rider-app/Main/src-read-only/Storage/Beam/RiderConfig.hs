{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RiderConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.RiderConfig
import qualified Domain.Types.RentalsIntercityCache
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
    cancellationPaymentDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    collectAutoCompleteData :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    collectMMIRouteData :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    csAlertTriggerDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    cxAgentDetails :: B.C f (Kernel.Prelude.Maybe [IssueManagement.Common.CxAgentDetails]),
    distanceWeightage :: B.C f Kernel.Prelude.Int,
    driverReferredSearchReqExpiry :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    dynamicLogicUpdatePassword :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    emailOtpConfig :: B.C f (Kernel.Prelude.Maybe Email.Types.EmailOTPConfig),
    enableEmergencyContactAddedMessage :: B.C f Kernel.Prelude.Bool,
    enableLocalPoliceSupport :: B.C f Kernel.Prelude.Bool,
    enableSupportForSafety :: B.C f Kernel.Prelude.Bool,
    excludedVehicleVariants :: B.C f (Kernel.Prelude.Maybe [Domain.Types.ServiceTierType.ServiceTierType]),
    executePaymentDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    exotelAppIdMapping :: B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.RiderConfig.ExotelMapping),
    exotelStatusCheckSchedulerDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    fareCacheInterCitySearchLocations :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    fareCacheRentalsConfig :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    feedbackAlertRatingThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    hardLimitForSafetyJobs :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    incidentReportSupport :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    intercitySearchLocations :: B.C f (Kernel.Prelude.Maybe [Domain.Types.RentalsIntercityCache.IntercitySearchLocation]),
    isAvoidToll :: B.C f Kernel.Prelude.Bool,
    isDeviceIdCheckDisabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isFirstReferredRideEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    ivrTriggerDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    kaptureConfig :: B.C f IssueManagement.Common.KaptureConfig,
    kaptureQueue :: B.C f Kernel.Prelude.Text,
    localPoliceNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    makeMultiModalSearch :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    maxAllowedPublicTransportLegs :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    maximumWalkDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    metroBookingAllowed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    minRidesToBlock :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    minRidesToShowCancellationRate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    minimumWalkDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    multimodalTesting :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    nearByDriverRingBucketCfg :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    payoutBatchDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    payoutBatchSize :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    payoutReferralProgram :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    payoutReferralStartDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    payoutReferralThresholdPerDay :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    payoutReferralThresholdPerMonth :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    permissibleModes :: B.C f (Kernel.Prelude.Maybe [Kernel.External.MultiModal.Interface.Types.GeneralVehicleType]),
    placeNameCacheExpiryDays :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    policeTriggerDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    postRideSafetyNotificationDelay :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    rentalsConfig :: B.C f (Kernel.Prelude.Maybe [Domain.Types.RentalsIntercityCache.RentalsConfig]),
    safetyCheckEndTime :: B.C f Kernel.Types.Common.Seconds,
    safetyCheckStartTime :: B.C f Kernel.Types.Common.Seconds,
    sensitiveWords :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    sensitiveWordsForExactMatch :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    settleCancellationFeeBeforeNextRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    specialZoneRadius :: B.C f Kernel.Prelude.Int,
    straightLineThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    suburbanBookingAllowed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    thresholdCancellationPercentageToBlock :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    timeDiffFromUtc :: B.C f Kernel.Types.Common.Seconds,
    trackingShortUrlPattern :: B.C f Kernel.Prelude.Text,
    useUserSettingsForSafetyIVR :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
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
