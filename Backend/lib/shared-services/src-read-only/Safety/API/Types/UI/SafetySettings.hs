{-# OPTIONS_GHC -Wno-unused-imports #-}

module Safety.API.Types.UI.SafetySettings where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Safety.Domain.Types.Common
import Servant

data GetSafetySettingsRes = GetSafetySettingsRes
  { aggregatedRideShareSetting :: Kernel.Prelude.Maybe Safety.Domain.Types.Common.RideShareOptions,
    autoCallDefaultContact :: Kernel.Prelude.Bool,
    enableOtpLessRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    enablePoliceSupport :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    enablePostRideSafetyCheck :: Safety.Domain.Types.Common.RideShareOptions,
    enableUnexpectedEventsCheck :: Safety.Domain.Types.Common.RideShareOptions,
    falseSafetyAlarmCount :: Kernel.Prelude.Int,
    hasCompletedMockSafetyDrill :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    hasCompletedSafetySetup :: Kernel.Prelude.Bool,
    informPoliceSos :: Kernel.Prelude.Bool,
    localPoliceNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    nightSafetyChecks :: Kernel.Prelude.Bool,
    notifySafetyTeamForSafetyCheckFailure :: Kernel.Prelude.Bool,
    notifySosWithEmergencyContacts :: Kernel.Prelude.Bool,
    safetyCenterDisabledOnDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    safetyCheckEndTime :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    safetyCheckStartTime :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    shakeToActivate :: Kernel.Prelude.Bool,
    shareEmergencyContacts :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    shareTripWithEmergencyContactOption :: Kernel.Prelude.Maybe Safety.Domain.Types.Common.RideShareOptions,
    shareTripWithEmergencyContacts :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateSafetySettingsReq = UpdateSafetySettingsReq
  { aggregatedRideShareSetting :: Kernel.Prelude.Maybe Safety.Domain.Types.Common.RideShareOptions,
    autoCallDefaultContact :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    enableOtpLessRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    enablePostRideSafetyCheck :: Kernel.Prelude.Maybe Safety.Domain.Types.Common.RideShareOptions,
    enableUnexpectedEventsCheck :: Kernel.Prelude.Maybe Safety.Domain.Types.Common.RideShareOptions,
    hasCompletedMockSafetyDrill :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    hasCompletedSafetySetup :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    informPoliceSos :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    nightSafetyChecks :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    notifySafetyTeamForSafetyCheckFailure :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    notifySosWithEmergencyContacts :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    shakeToActivate :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    shareEmergencyContacts :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    shareTripWithEmergencyContactOption :: Kernel.Prelude.Maybe Safety.Domain.Types.Common.RideShareOptions,
    shareTripWithEmergencyContacts :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
