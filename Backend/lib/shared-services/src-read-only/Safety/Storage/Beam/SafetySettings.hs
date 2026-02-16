{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Safety.Storage.Beam.SafetySettings where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Safety.Domain.Types.Common

data SafetySettingsT f = SafetySettingsT
  { aggregatedRideShareSetting :: (B.C f (Kernel.Prelude.Maybe Safety.Domain.Types.Common.RideShareOptions)),
    autoCallDefaultContact :: (B.C f Kernel.Prelude.Bool),
    enableOtpLessRide :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    enablePostRideSafetyCheck :: (B.C f Safety.Domain.Types.Common.RideShareOptions),
    enableUnexpectedEventsCheck :: (B.C f Safety.Domain.Types.Common.RideShareOptions),
    falseSafetyAlarmCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    hasCompletedMockSafetyDrill :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    hasCompletedSafetySetup :: (B.C f Kernel.Prelude.Bool),
    informPoliceSos :: (B.C f Kernel.Prelude.Bool),
    nightSafetyChecks :: (B.C f Kernel.Prelude.Bool),
    notifySafetyTeamForSafetyCheckFailure :: (B.C f Kernel.Prelude.Bool),
    notifySosWithEmergencyContacts :: (B.C f Kernel.Prelude.Bool),
    personId :: (B.C f Kernel.Prelude.Text),
    safetyCenterDisabledOnDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    shakeToActivate :: (B.C f Kernel.Prelude.Bool)
  }
  deriving (Generic, B.Beamable)

instance B.Table SafetySettingsT where
  data PrimaryKey SafetySettingsT f = SafetySettingsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SafetySettingsId . personId

type SafetySettings = SafetySettingsT Identity

$(enableKVPG (''SafetySettingsT) [('personId)] [])

$(mkTableInstancesGenericSchema (''SafetySettingsT) "safety_settings")
