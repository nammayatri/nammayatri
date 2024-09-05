{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SafetySettings where

import qualified Domain.Types.SafetySettings
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.SafetySettings as Beam

instance FromTType' Beam.SafetySettings Domain.Types.SafetySettings.SafetySettings where
  fromTType' (Beam.SafetySettingsT {..}) = do
    pure $
      Just
        Domain.Types.SafetySettings.SafetySettings
          { aggregatedRideShareSetting = aggregatedRideShareSetting,
            autoCallDefaultContact = autoCallDefaultContact,
            enableOtpLessRide = enableOtpLessRide,
            enablePostRideSafetyCheck = enablePostRideSafetyCheck,
            enableUnexpectedEventsCheck = enableUnexpectedEventsCheck,
            falseSafetyAlarmCount = fromMaybe 0 falseSafetyAlarmCount,
            hasCompletedMockSafetyDrill = hasCompletedMockSafetyDrill,
            hasCompletedSafetySetup = hasCompletedSafetySetup,
            informPoliceSos = informPoliceSos,
            nightSafetyChecks = nightSafetyChecks,
            notifySafetyTeamForSafetyCheckFailure = notifySafetyTeamForSafetyCheckFailure,
            notifySosWithEmergencyContacts = notifySosWithEmergencyContacts,
            personId = Kernel.Types.Id.Id personId,
            safetyCenterDisabledOnDate = safetyCenterDisabledOnDate,
            shakeToActivate = shakeToActivate,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SafetySettings Domain.Types.SafetySettings.SafetySettings where
  toTType' (Domain.Types.SafetySettings.SafetySettings {..}) = do
    Beam.SafetySettingsT
      { Beam.aggregatedRideShareSetting = aggregatedRideShareSetting,
        Beam.autoCallDefaultContact = autoCallDefaultContact,
        Beam.enableOtpLessRide = enableOtpLessRide,
        Beam.enablePostRideSafetyCheck = enablePostRideSafetyCheck,
        Beam.enableUnexpectedEventsCheck = enableUnexpectedEventsCheck,
        Beam.falseSafetyAlarmCount = Just falseSafetyAlarmCount,
        Beam.hasCompletedMockSafetyDrill = hasCompletedMockSafetyDrill,
        Beam.hasCompletedSafetySetup = hasCompletedSafetySetup,
        Beam.informPoliceSos = informPoliceSos,
        Beam.nightSafetyChecks = nightSafetyChecks,
        Beam.notifySafetyTeamForSafetyCheckFailure = notifySafetyTeamForSafetyCheckFailure,
        Beam.notifySosWithEmergencyContacts = notifySosWithEmergencyContacts,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.safetyCenterDisabledOnDate = safetyCenterDisabledOnDate,
        Beam.shakeToActivate = shakeToActivate,
        Beam.updatedAt = updatedAt
      }
