{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RiderConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Email.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RiderConfig = RiderConfig
  { appUrl :: Kernel.Prelude.Text,
    autoUnblockSafetyCenterAfterDays :: Kernel.Prelude.Int,
    bookingSyncStatusCallSecondsDiffThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    collectAutoCompleteData :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    collectMMIRouteData :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    distanceWeightage :: Kernel.Prelude.Int,
    emailOtpConfig :: Kernel.Prelude.Maybe Email.Types.EmailOTPConfig,
    enableEmergencyContactAddedMessage :: Kernel.Prelude.Bool,
    enableLocalPoliceSupport :: Kernel.Prelude.Bool,
    enableSupportForSafety :: Kernel.Prelude.Bool,
    isAvoidToll :: Kernel.Prelude.Bool,
    kaptureQueue :: Kernel.Prelude.Text,
    localPoliceNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    placeNameCacheExpiryDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    safetyCheckEndTime :: Kernel.Types.Common.Seconds,
    safetyCheckStartTime :: Kernel.Types.Common.Seconds,
    shouldBlockedBySameDeviceToken :: Kernel.Prelude.Bool,
    specialZoneRadius :: Kernel.Prelude.Int,
    timeDiffFromUtc :: Kernel.Types.Common.Seconds,
    trackingShortUrlPattern :: Kernel.Prelude.Text,
    videoFileSizeUpperLimit :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
