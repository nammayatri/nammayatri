{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RiderConfig (module Domain.Types.RiderConfig, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.RiderConfig as ReExport
import qualified Domain.Types.Extra.RiderConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
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
    executePaymentDelay :: Kernel.Prelude.NominalDiffTime,
    exotelAppIdMapping :: Kernel.Prelude.Maybe Domain.Types.Extra.RiderConfig.ExotelMapping,
    exotelStatusCheckSchedulerDelay :: Kernel.Prelude.Int,
    feedbackAlertRatingThreshold :: Kernel.Prelude.Int,
    hardLimitForSafetyJobs :: Kernel.Prelude.Int,
    incidentReportSupport :: Kernel.Prelude.Bool,
    isAvoidToll :: Kernel.Prelude.Bool,
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
    payoutBatchDelay :: Kernel.Prelude.NominalDiffTime,
    payoutBatchSize :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    payoutReferralProgram :: Kernel.Prelude.Bool,
    payoutReferralStartDate :: Kernel.Prelude.UTCTime,
    permissibleModes :: Kernel.Prelude.Maybe [Kernel.External.MultiModal.Interface.Types.GeneralVehicleType],
    placeNameCacheExpiryDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    policeTriggerDelay :: Kernel.Prelude.NominalDiffTime,
    postRideSafetyNotificationDelay :: Kernel.Prelude.NominalDiffTime,
    safetyCheckEndTime :: Kernel.Types.Common.Seconds,
    safetyCheckStartTime :: Kernel.Types.Common.Seconds,
    sensitiveWords :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    sensitiveWordsForExactMatch :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    settleCancellationFeeBeforeNextRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    specialZoneRadius :: Kernel.Prelude.Int,
    thresholdCancellationPercentageToBlock :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    timeDiffFromUtc :: Kernel.Types.Common.Seconds,
    trackingShortUrlPattern :: Kernel.Prelude.Text,
    useUserSettingsForSafetyIVR :: Kernel.Prelude.Bool,
    videoFileSizeUpperLimit :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)
