{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SafetySettings where

import Data.Aeson
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data SafetySettings = SafetySettings
  { aggregatedRideShareSetting :: Kernel.Prelude.Maybe Domain.Types.Person.RideShareOptions,
    autoCallDefaultContact :: Kernel.Prelude.Bool,
    enableOtpLessRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    enablePostRideSafetyCheck :: Domain.Types.Person.RideShareOptions,
    enableUnexpectedEventsCheck :: Domain.Types.Person.RideShareOptions,
    falseSafetyAlarmCount :: Kernel.Prelude.Int,
    hasCompletedMockSafetyDrill :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    hasCompletedSafetySetup :: Kernel.Prelude.Bool,
    informPoliceSos :: Kernel.Prelude.Bool,
    nightSafetyChecks :: Kernel.Prelude.Bool,
    notifySafetyTeamForSafetyCheckFailure :: Kernel.Prelude.Bool,
    notifySosWithEmergencyContacts :: Kernel.Prelude.Bool,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    safetyCenterDisabledOnDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    shakeToActivate :: Kernel.Prelude.Bool,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
