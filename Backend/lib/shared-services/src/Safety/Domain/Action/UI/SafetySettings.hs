module Safety.Domain.Action.UI.SafetySettings
  ( ServiceHandle (..),
    SafetyRiderExtras (..),
    HasSafetySettingsHandle (..),
    BuildSafetySettingsCtx (..),
    getSafetySettings,
    updateSafetySettings,
  )
where

import Control.Applicative ((<|>))
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Safety.API.Types.UI.SafetySettings as API
import qualified Safety.Domain.Types.Common as SafetyCommon
import Safety.Storage.BeamFlow
import qualified Safety.Storage.Queries.SafetySettingsExtra as QSafetyExtra

-- | Rider-specific extras; driver provides Nothing for all.
data SafetyRiderExtras = SafetyRiderExtras
  { shareTripWithEmergencyContacts :: Bool,
    shareTripWithEmergencyContactOption :: SafetyCommon.RideShareOptions,
    enablePoliceSupport :: Bool,
    localPoliceNumber :: Maybe Text,
    safetyCheckStartTime :: Seconds,
    safetyCheckEndTime :: Seconds
  }

data ServiceHandle m = ServiceHandle
  { -- | Fetch both person defaults and rider-specific extras in one call.
    -- Rider: fetches Person row once, returns both.
    -- Driver: pure (Nothing, Nothing).
    getPlatformExtras :: Id SafetyCommon.Person -> m (Maybe QSafetyExtra.SafetySettingsPersonDefaults, Maybe SafetyRiderExtras),
    -- | Platform-specific side-effects for an update request.
    -- Rider: validates emergency contacts are non-empty when required, updates personEN table.
    -- Driver: pure ().
    handlePlatformUpdate :: Id SafetyCommon.Person -> API.UpdateSafetySettingsReq -> m ()
  }

class HasSafetySettingsHandle r m | m -> r where
  getSafetySettingsHandle :: m (ServiceHandle m)

-- | Extract a safety-domain personId from an auth token.
-- Instances live in shared-services so they are never orphan.
class BuildSafetySettingsCtx authToken m where
  extractSafetyPersonId :: authToken -> m (Id SafetyCommon.Person)

-- Driver: (Id Person, Id Merchant, Id MerchantOpCity)
instance Monad m => BuildSafetySettingsCtx (Id a, Id b, Id c) m where
  extractSafetyPersonId (personId, _, _) = pure (cast personId)

-- Rider: (Id Person, Id Merchant)
instance Monad m => BuildSafetySettingsCtx (Id a, Id b) m where
  extractSafetyPersonId (personId, _) = pure (cast personId)

getSafetySettings ::
  (BeamFlow m r, MonadFlow m, HasSafetySettingsHandle r m, BuildSafetySettingsCtx authToken m) =>
  authToken ->
  m API.GetSafetySettingsRes
getSafetySettings authToken = do
  personId <- extractSafetyPersonId authToken
  svcHandle <- getSafetySettingsHandle
  (mbDefaults, mbRiderExtras) <- svcHandle.getPlatformExtras personId
  safetySettings <- QSafetyExtra.findSafetySettingsWithFallback personId (QSafetyExtra.getDefaultSafetySettings personId mbDefaults)
  pure
    API.GetSafetySettingsRes
      { aggregatedRideShareSetting = safetySettings.aggregatedRideShareSetting,
        autoCallDefaultContact = safetySettings.autoCallDefaultContact,
        enableOtpLessRide = safetySettings.enableOtpLessRide,
        enablePostRideSafetyCheck = safetySettings.enablePostRideSafetyCheck,
        enableUnexpectedEventsCheck = safetySettings.enableUnexpectedEventsCheck,
        falseSafetyAlarmCount = safetySettings.falseSafetyAlarmCount,
        hasCompletedMockSafetyDrill = safetySettings.hasCompletedMockSafetyDrill,
        hasCompletedSafetySetup = safetySettings.hasCompletedSafetySetup,
        informPoliceSos = safetySettings.informPoliceSos,
        nightSafetyChecks = safetySettings.nightSafetyChecks,
        notifySafetyTeamForSafetyCheckFailure = safetySettings.notifySafetyTeamForSafetyCheckFailure,
        notifySosWithEmergencyContacts = safetySettings.notifySosWithEmergencyContacts,
        safetyCenterDisabledOnDate = safetySettings.safetyCenterDisabledOnDate,
        shakeToActivate = safetySettings.shakeToActivate,
        -- shareEmergencyContacts is an alias for notifySosWithEmergencyContacts (rider compat)
        shareEmergencyContacts = Just safetySettings.notifySosWithEmergencyContacts,
        shareTripWithEmergencyContacts = (.shareTripWithEmergencyContacts) <$> mbRiderExtras,
        shareTripWithEmergencyContactOption = (.shareTripWithEmergencyContactOption) <$> mbRiderExtras,
        enablePoliceSupport = (.enablePoliceSupport) <$> mbRiderExtras,
        localPoliceNumber = mbRiderExtras >>= (.localPoliceNumber),
        safetyCheckStartTime = (.safetyCheckStartTime) <$> mbRiderExtras,
        safetyCheckEndTime = (.safetyCheckEndTime) <$> mbRiderExtras
      }

updateSafetySettings ::
  (BeamFlow m r, MonadFlow m, HasSafetySettingsHandle r m, BuildSafetySettingsCtx authToken m) =>
  authToken ->
  API.UpdateSafetySettingsReq ->
  m APISuccess.APISuccess
updateSafetySettings authToken req = do
  personId <- extractSafetyPersonId authToken
  svcHandle <- getSafetySettingsHandle
  (mbDefaults, _) <- svcHandle.getPlatformExtras personId
  -- shareEmergencyContacts acts as a fallback for autoCallDefaultContact and
  -- notifySosWithEmergencyContacts when those fields are not explicitly set.
  let setContactField field = field <|> req.shareEmergencyContacts
      emergencyInfo =
        QSafetyExtra.UpdateEmergencyInfo
          { autoCallDefaultContact = setContactField req.autoCallDefaultContact,
            enablePostRideSafetyCheck = req.enablePostRideSafetyCheck,
            enableUnexpectedEventsCheck = req.enableUnexpectedEventsCheck,
            hasCompletedMockSafetyDrill = req.hasCompletedMockSafetyDrill,
            hasCompletedSafetySetup = req.hasCompletedSafetySetup,
            informPoliceSos = req.informPoliceSos,
            nightSafetyChecks = req.nightSafetyChecks,
            notifySafetyTeamForSafetyCheckFailure = req.notifySafetyTeamForSafetyCheckFailure,
            notifySosWithEmergencyContacts = setContactField req.notifySosWithEmergencyContacts,
            shakeToActivate = req.shakeToActivate,
            enableOtpLessRide = req.enableOtpLessRide,
            aggregatedRideShare = req.aggregatedRideShareSetting
          }
  svcHandle.handlePlatformUpdate personId req
  QSafetyExtra.upsert personId emergencyInfo (QSafetyExtra.getDefaultSafetySettings personId mbDefaults)
  pure APISuccess.Success
