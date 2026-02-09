{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Safety.Domain.Types.SafetySettings where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Safety.Domain.Types.Common

data SafetySettings = SafetySettings
  { aggregatedRideShareSetting :: Kernel.Prelude.Maybe Safety.Domain.Types.Common.RideShareOptions,
    autoCallDefaultContact :: Kernel.Prelude.Bool,
    enableOtpLessRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    enablePostRideSafetyCheck :: Safety.Domain.Types.Common.RideShareOptions,
    enableUnexpectedEventsCheck :: Safety.Domain.Types.Common.RideShareOptions,
    falseSafetyAlarmCount :: Kernel.Prelude.Int,
    hasCompletedMockSafetyDrill :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    hasCompletedSafetySetup :: Kernel.Prelude.Bool,
    informPoliceSos :: Kernel.Prelude.Bool,
    nightSafetyChecks :: Kernel.Prelude.Bool,
    notifySafetyTeamForSafetyCheckFailure :: Kernel.Prelude.Bool,
    notifySosWithEmergencyContacts :: Kernel.Prelude.Bool,
    personId :: Kernel.Types.Id.Id Safety.Domain.Types.Common.Person,
    safetyCenterDisabledOnDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    shakeToActivate :: Kernel.Prelude.Bool
  }
  deriving (Generic)
