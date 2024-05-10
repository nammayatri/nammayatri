{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RiderConfig where

import qualified Database.Beam as B
import qualified Email.Types
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data RiderConfigT f = RiderConfigT
  { appUrl :: (B.C f Kernel.Prelude.Text),
    autoUnblockSafetyCenterAfterDays :: (B.C f Kernel.Prelude.Int),
    bookingSyncStatusCallSecondsDiffThreshold :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    collectAutoCompleteData :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    collectMMIRouteData :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    distanceWeightage :: (B.C f Kernel.Prelude.Int),
    emailOtpConfig :: (B.C f (Kernel.Prelude.Maybe Email.Types.EmailOTPConfig)),
    enableEmergencyContactAddedMessage :: (B.C f Kernel.Prelude.Bool),
    enableLocalPoliceSupport :: (B.C f Kernel.Prelude.Bool),
    enableSupportForSafety :: (B.C f Kernel.Prelude.Bool),
    isAvoidToll :: (B.C f Kernel.Prelude.Bool),
    kaptureQueue :: (B.C f Kernel.Prelude.Text),
    localPoliceNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    placeNameCacheExpiryDays :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    safetyCheckEndTime :: (B.C f Kernel.Types.Common.Seconds),
    safetyCheckStartTime :: (B.C f Kernel.Types.Common.Seconds),
    shouldBlockedBySameDeviceToken :: B.C f Kernel.Prelude.Bool,
    specialZoneRadius :: (B.C f Kernel.Prelude.Int),
    timeDiffFromUtc :: (B.C f Kernel.Types.Common.Seconds),
    trackingShortUrlPattern :: (B.C f Kernel.Prelude.Text),
    videoFileSizeUpperLimit :: (B.C f Kernel.Prelude.Int),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RiderConfigT where
  data PrimaryKey RiderConfigT f = RiderConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RiderConfigId . merchantOperatingCityId

type RiderConfig = RiderConfigT Identity

$(enableKVPG (''RiderConfigT) [('merchantOperatingCityId)] [])

$(mkTableInstances (''RiderConfigT) "rider_config")
