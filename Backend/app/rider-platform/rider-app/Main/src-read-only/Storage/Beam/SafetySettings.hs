{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SafetySettings where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SafetySettingsT f = SafetySettingsT
  { aggregatedRideShareSetting :: B.C f (Kernel.Prelude.Maybe Domain.Types.Person.RideShareOptions),
    autoCallDefaultContact :: B.C f Kernel.Prelude.Bool,
    enableOtpLessRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    enablePostRideSafetyCheck :: B.C f Domain.Types.Person.RideShareOptions,
    enableUnexpectedEventsCheck :: B.C f Domain.Types.Person.RideShareOptions,
    falseSafetyAlarmCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    hasCompletedMockSafetyDrill :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    hasCompletedSafetySetup :: B.C f Kernel.Prelude.Bool,
    informPoliceSos :: B.C f Kernel.Prelude.Bool,
    nightSafetyChecks :: B.C f Kernel.Prelude.Bool,
    notifySafetyTeamForSafetyCheckFailure :: B.C f Kernel.Prelude.Bool,
    notifySosWithEmergencyContacts :: B.C f Kernel.Prelude.Bool,
    personId :: B.C f Kernel.Prelude.Text,
    safetyCenterDisabledOnDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    shakeToActivate :: B.C f Kernel.Prelude.Bool,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SafetySettingsT where
  data PrimaryKey SafetySettingsT f = SafetySettingsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SafetySettingsId . personId

type SafetySettings = SafetySettingsT Identity

$(enableKVPG ''SafetySettingsT ['personId] [])

$(mkTableInstances ''SafetySettingsT "safety_settings")
