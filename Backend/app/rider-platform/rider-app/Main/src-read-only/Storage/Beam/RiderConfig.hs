{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RiderConfig where

import qualified Database.Beam as B
import qualified Email.Types
import qualified IssueManagement.Common
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data RiderConfigT f = RiderConfigT
  { merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    enableLocalPoliceSupport :: B.C f Kernel.Prelude.Bool,
    localPoliceNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    enableSupportForSafety :: B.C f Kernel.Prelude.Bool,
    videoFileSizeUpperLimit :: B.C f Kernel.Prelude.Int,
    timeDiffFromUtc :: B.C f Kernel.Types.Common.Seconds,
    enableEmergencyContactAddedMessage :: B.C f Kernel.Prelude.Bool,
    safetyCheckStartTime :: B.C f Kernel.Types.Common.Seconds,
    safetyCheckEndTime :: B.C f Kernel.Types.Common.Seconds,
    trackingShortUrlPattern :: B.C f Kernel.Prelude.Text,
    specialZoneRadius :: B.C f Kernel.Prelude.Int,
    appUrl :: B.C f Kernel.Prelude.Text,
    collectAutoCompleteData :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    distanceWeightage :: B.C f Kernel.Prelude.Int,
    collectMMIRouteData :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isAvoidToll :: B.C f Kernel.Prelude.Bool,
    autoUnblockSafetyCenterAfterDays :: B.C f Kernel.Prelude.Int,
    placeNameCacheExpiryDays :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    bookingSyncStatusCallSecondsDiffThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    kaptureQueue :: B.C f Kernel.Prelude.Text,
    kaptureConfig :: B.C f IssueManagement.Common.KaptureConfig,
    emailOtpConfig :: B.C f (Kernel.Prelude.Maybe Email.Types.EmailOTPConfig),
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
