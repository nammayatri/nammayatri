{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Safety.Storage.Queries.OrphanInstances.SafetySettings where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Safety.Domain.Types.SafetySettings
import qualified Safety.Storage.Beam.SafetySettings as Beam

instance FromTType' Beam.SafetySettings Safety.Domain.Types.SafetySettings.SafetySettings where
  fromTType' (Beam.SafetySettingsT {..}) = do
    pure $
      Just
        Safety.Domain.Types.SafetySettings.SafetySettings
          { aggregatedRideShareSetting = aggregatedRideShareSetting,
            autoCallDefaultContact = autoCallDefaultContact,
            enableOtpLessRide = enableOtpLessRide,
            enablePostRideSafetyCheck = enablePostRideSafetyCheck,
            enableUnexpectedEventsCheck = enableUnexpectedEventsCheck,
            falseSafetyAlarmCount = Kernel.Prelude.fromMaybe 0 falseSafetyAlarmCount,
            hasCompletedMockSafetyDrill = hasCompletedMockSafetyDrill,
            hasCompletedSafetySetup = hasCompletedSafetySetup,
            informPoliceSos = informPoliceSos,
            nightSafetyChecks = nightSafetyChecks,
            notifySafetyTeamForSafetyCheckFailure = notifySafetyTeamForSafetyCheckFailure,
            notifySosWithEmergencyContacts = notifySosWithEmergencyContacts,
            personId = Kernel.Types.Id.Id personId,
            safetyCenterDisabledOnDate = safetyCenterDisabledOnDate,
            shakeToActivate = shakeToActivate
          }

instance ToTType' Beam.SafetySettings Safety.Domain.Types.SafetySettings.SafetySettings where
  toTType' (Safety.Domain.Types.SafetySettings.SafetySettings {..}) = do
    Beam.SafetySettingsT
      { Beam.aggregatedRideShareSetting = aggregatedRideShareSetting,
        Beam.autoCallDefaultContact = autoCallDefaultContact,
        Beam.enableOtpLessRide = enableOtpLessRide,
        Beam.enablePostRideSafetyCheck = enablePostRideSafetyCheck,
        Beam.enableUnexpectedEventsCheck = enableUnexpectedEventsCheck,
        Beam.falseSafetyAlarmCount = (Kernel.Prelude.Just falseSafetyAlarmCount),
        Beam.hasCompletedMockSafetyDrill = hasCompletedMockSafetyDrill,
        Beam.hasCompletedSafetySetup = hasCompletedSafetySetup,
        Beam.informPoliceSos = informPoliceSos,
        Beam.nightSafetyChecks = nightSafetyChecks,
        Beam.notifySafetyTeamForSafetyCheckFailure = notifySafetyTeamForSafetyCheckFailure,
        Beam.notifySosWithEmergencyContacts = notifySosWithEmergencyContacts,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.safetyCenterDisabledOnDate = safetyCenterDisabledOnDate,
        Beam.shakeToActivate = shakeToActivate
      }
