{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SafetySettingsExtra where

import qualified Domain.Types.Person as Person
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow)
import qualified Safety.Domain.Types.Common as SafetyCommon
import qualified Safety.Domain.Types.SafetySettings as DSafety
import qualified Safety.Storage.BeamFlow
import qualified Safety.Storage.Queries.SafetySettings as QSafetySettings
import Storage.Beam.Sos ()

-- | Find safety_settings for the given driver person; if none exists, create a row with
-- defaults (driver Person does not have safety-related columns) and return it.
-- Ensures markSosAsSafe and other shared safety flows have a row to work with.
findSafetySettingsWithFallback ::
  (MonadFlow m, Safety.Storage.BeamFlow.BeamFlow m r) =>
  Id Person.Person ->
  m DSafety.SafetySettings
findSafetySettingsWithFallback personId = do
  let safetyPersonId = cast personId
  mb <- QSafetySettings.findByPersonId safetyPersonId
  case mb of
    Just ss -> return ss
    Nothing -> do
      let safetySettings =
            DSafety.SafetySettings
              { aggregatedRideShareSetting = Nothing,
                autoCallDefaultContact = False,
                enableOtpLessRide = Nothing,
                enablePostRideSafetyCheck = SafetyCommon.NEVER_SHARE,
                enableUnexpectedEventsCheck = SafetyCommon.NEVER_SHARE,
                falseSafetyAlarmCount = 0,
                hasCompletedMockSafetyDrill = Nothing,
                hasCompletedSafetySetup = False,
                informPoliceSos = False,
                nightSafetyChecks = False,
                notifySafetyTeamForSafetyCheckFailure = False,
                notifySosWithEmergencyContacts = True,
                personId = safetyPersonId,
                safetyCenterDisabledOnDate = Nothing,
                shakeToActivate = False
              }
      QSafetySettings.create safetySettings
      return safetySettings
