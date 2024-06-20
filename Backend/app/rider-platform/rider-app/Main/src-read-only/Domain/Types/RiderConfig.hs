{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RiderConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Email.Types
import qualified IssueManagement.Common
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RiderConfig = RiderConfig
  { merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    enableLocalPoliceSupport :: Kernel.Prelude.Bool,
    localPoliceNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enableSupportForSafety :: Kernel.Prelude.Bool,
    videoFileSizeUpperLimit :: Kernel.Prelude.Int,
    timeDiffFromUtc :: Kernel.Types.Common.Seconds,
    enableEmergencyContactAddedMessage :: Kernel.Prelude.Bool,
    safetyCheckStartTime :: Kernel.Types.Common.Seconds,
    safetyCheckEndTime :: Kernel.Types.Common.Seconds,
    trackingShortUrlPattern :: Kernel.Prelude.Text,
    specialZoneRadius :: Kernel.Prelude.Int,
    appUrl :: Kernel.Prelude.Text,
    collectAutoCompleteData :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    distanceWeightage :: Kernel.Prelude.Int,
    collectMMIRouteData :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isAvoidToll :: Kernel.Prelude.Bool,
    autoUnblockSafetyCenterAfterDays :: Kernel.Prelude.Int,
    placeNameCacheExpiryDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bookingSyncStatusCallSecondsDiffThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    kaptureQueue :: Kernel.Prelude.Text,
    kaptureConfig :: IssueManagement.Common.KaptureConfig,
    emailOtpConfig :: Kernel.Prelude.Maybe Email.Types.EmailOTPConfig,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
