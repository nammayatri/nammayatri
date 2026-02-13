{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverSafetySettings where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Person
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Safety.Domain.Types.Common
import Servant
import Tools.Auth

data GetDriverSafetySettingsRes = GetDriverSafetySettingsRes
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
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    safetyCenterDisabledOnDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    shakeToActivate :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateDriverSafetySettingsReq = UpdateDriverSafetySettingsReq
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
    shakeToActivate :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
