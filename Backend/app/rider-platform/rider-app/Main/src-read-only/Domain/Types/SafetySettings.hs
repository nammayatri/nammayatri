{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SafetySettings where

import Data.Aeson
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data SafetySettings = SafetySettings
  { autoCallDefaultContact :: Kernel.Prelude.Bool,
    enableOtpLessRide :: Kernel.Prelude.Bool,
    enablePostRideSafetyCheck :: Kernel.Prelude.Bool,
    enableUnexpectedEventsCheck :: Kernel.Prelude.Bool,
    falseSafetyAlarmCount :: Kernel.Prelude.Int,
    hasCompletedMockSafetyDrill :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    hasCompletedSafetySetup :: Kernel.Prelude.Bool,
    hasSetupRideOtp :: Kernel.Prelude.Bool,
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
