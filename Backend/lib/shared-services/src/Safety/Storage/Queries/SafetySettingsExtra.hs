{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Safety.Storage.Queries.SafetySettingsExtra
  ( UpdateEmergencyInfo (..),
    SafetySettingsPersonDefaults (..),
    emptyUpdateEmergencyInfo,
    findSafetySettingsWithFallback,
    getDefaultSafetySettings,
    upsert,
    updateSafetyCenterBlockingCounter,
    mkSafetySettingsByPersonIdKey,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.OpenApi (ToSchema)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Safety.Domain.Types.Common as SafetyCommon
import qualified Safety.Domain.Types.PersonDefaultEmergencyNumber as SafetyPDEN
import qualified Safety.Domain.Types.SafetySettings as DSafety
import qualified Safety.Storage.Beam.SafetySettings as BeamP
import Safety.Storage.BeamFlow
import Safety.Storage.Queries.OrphanInstances.SafetySettings ()
import qualified Safety.Storage.Queries.PersonDefaultEmergencyNumber as LibQPDEN
import qualified Sequelize as Se

-- | App-agnostic update payload for safety settings. Used by both rider and driver.
data UpdateEmergencyInfo = UpdateEmergencyInfo
  { autoCallDefaultContact :: Maybe Bool,
    enablePostRideSafetyCheck :: Maybe SafetyCommon.RideShareOptions,
    enableUnexpectedEventsCheck :: Maybe SafetyCommon.RideShareOptions,
    hasCompletedMockSafetyDrill :: Maybe Bool,
    hasCompletedSafetySetup :: Maybe Bool,
    informPoliceSos :: Maybe Bool,
    nightSafetyChecks :: Maybe Bool,
    notifySafetyTeamForSafetyCheckFailure :: Maybe Bool,
    notifySosWithEmergencyContacts :: Maybe Bool,
    shakeToActivate :: Maybe Bool,
    enableOtpLessRide :: Maybe Bool,
    aggregatedRideShare :: Maybe SafetyCommon.RideShareOptions
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

emptyUpdateEmergencyInfo :: UpdateEmergencyInfo
emptyUpdateEmergencyInfo =
  UpdateEmergencyInfo
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

-- | Person-derived defaults for safety settings. Rider and driver apps build this from
-- their Person type and pass to getDefaultSafetySettings for backward compatibility.
data SafetySettingsPersonDefaults = SafetySettingsPersonDefaults
  { nightSafetyChecks :: Bool,
    shareEmergencyContacts :: Bool,
    falseSafetyAlarmCount :: Int,
    hasCompletedMockSafetyDrill :: Maybe Bool,
    hasCompletedSafetySetup :: Bool,
    informPoliceSos :: Bool,
    notifySafetyTeamForSafetyCheckFailure :: Bool,
    notifySosWithEmergencyContacts :: Bool,
    shakeToActivate :: Bool,
    enableOtpLessRide :: Maybe Bool,
    safetyCenterDisabledOnDate :: Maybe UTCTime,
    shareTripWithEmergencyContactOption :: Maybe SafetyCommon.RideShareOptions
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Default safety settings. When mbPersonDefaults is Nothing, uses fixed defaults and
-- aggregatedRideShareSetting from PDEN. When Just, uses the passed person-derived defaults.
getDefaultSafetySettings ::
  (BeamFlow m r, MonadFlow m) =>
  Id SafetyCommon.Person ->
  Maybe SafetySettingsPersonDefaults ->
  m DSafety.SafetySettings
getDefaultSafetySettings personId mbPersonDefaults = do
  now <- getCurrentTime
  case mbPersonDefaults of
    Nothing -> do
      personENList <- LibQPDEN.findAllByPersonId personId
      let aggregatedRideShareSetting = listToMaybe personENList >>= SafetyPDEN.shareTripWithEmergencyContactOption
      pure $
        DSafety.SafetySettings
          { aggregatedRideShareSetting,
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
            personId,
            safetyCenterDisabledOnDate = Nothing,
            shakeToActivate = False,
            updatedAt = now
          }
    Just p ->
      pure $
        DSafety.SafetySettings
          { aggregatedRideShareSetting = p.shareTripWithEmergencyContactOption,
            autoCallDefaultContact = p.shareEmergencyContacts,
            enableOtpLessRide = p.enableOtpLessRide,
            enablePostRideSafetyCheck = SafetyCommon.NEVER_SHARE,
            enableUnexpectedEventsCheck = SafetyCommon.NEVER_SHARE,
            falseSafetyAlarmCount = p.falseSafetyAlarmCount,
            hasCompletedMockSafetyDrill = p.hasCompletedMockSafetyDrill,
            hasCompletedSafetySetup = p.hasCompletedSafetySetup,
            informPoliceSos = p.informPoliceSos,
            nightSafetyChecks = p.nightSafetyChecks,
            notifySafetyTeamForSafetyCheckFailure = p.notifySafetyTeamForSafetyCheckFailure,
            notifySosWithEmergencyContacts = p.notifySosWithEmergencyContacts,
            personId,
            safetyCenterDisabledOnDate = p.safetyCenterDisabledOnDate,
            shakeToActivate = p.shakeToActivate,
            updatedAt = now
          }

-- | Find safety_settings for the given person; if none exists, run the provided
-- action to get default settings, create a row, and return it.
-- Caller (rider or driver app) supplies the default via the second argument.
findSafetySettingsWithFallback ::
  (BeamFlow m r, MonadFlow m) =>
  Id SafetyCommon.Person ->
  m DSafety.SafetySettings ->
  m DSafety.SafetySettings
findSafetySettingsWithFallback personId getDefaultSettings =
  Hedis.withLockRedisAndReturnValue (mkSafetySettingsByPersonIdKey $ getId personId) 1 $ do
    res <- findOneWithKV [Se.And [Se.Is BeamP.personId $ Se.Eq $ getId personId]]
    case res of
      Just safetySettings -> return safetySettings
      Nothing -> do
        safetySettings <- getDefaultSettings
        _ <- createWithKV safetySettings
        return safetySettings

-- | Upsert safety settings: update existing row or create using the provided default.
-- Caller supplies an action that returns the full default SafetySettings when creating.
upsert ::
  (BeamFlow m r, MonadFlow m) =>
  Id SafetyCommon.Person ->
  UpdateEmergencyInfo ->
  m DSafety.SafetySettings ->
  m ()
upsert personId UpdateEmergencyInfo {..} getDefaultSettings =
  Hedis.withLockRedis (mkSafetySettingsByPersonIdKey $ getId personId) 1 $ do
    now <- getCurrentTime
    res <- findOneWithKV [Se.And [Se.Is BeamP.personId $ Se.Eq $ getId personId]]
    if isJust res
      then
        updateWithKV
          ( [Se.Set BeamP.updatedAt now]
              <> [Se.Set BeamP.autoCallDefaultContact (fromJust autoCallDefaultContact) | isJust autoCallDefaultContact]
              <> [Se.Set BeamP.enablePostRideSafetyCheck (fromJust enablePostRideSafetyCheck) | isJust enablePostRideSafetyCheck]
              <> [Se.Set BeamP.enableUnexpectedEventsCheck (fromJust enableUnexpectedEventsCheck) | isJust enableUnexpectedEventsCheck]
              <> [Se.Set BeamP.hasCompletedMockSafetyDrill hasCompletedMockSafetyDrill | isJust hasCompletedMockSafetyDrill]
              <> [Se.Set BeamP.hasCompletedSafetySetup (fromJust hasCompletedSafetySetup) | isJust hasCompletedSafetySetup && hasCompletedSafetySetup == Just True]
              <> [Se.Set BeamP.informPoliceSos (fromJust informPoliceSos) | isJust informPoliceSos]
              <> [Se.Set BeamP.nightSafetyChecks (fromJust nightSafetyChecks) | isJust nightSafetyChecks]
              <> [Se.Set BeamP.notifySafetyTeamForSafetyCheckFailure (fromJust notifySafetyTeamForSafetyCheckFailure) | isJust notifySafetyTeamForSafetyCheckFailure]
              <> [Se.Set BeamP.notifySosWithEmergencyContacts (fromJust notifySosWithEmergencyContacts) | isJust notifySosWithEmergencyContacts]
              <> [Se.Set BeamP.shakeToActivate (fromJust shakeToActivate) | isJust shakeToActivate]
              <> [Se.Set BeamP.enableOtpLessRide enableOtpLessRide | isJust enableOtpLessRide]
              <> [Se.Set BeamP.aggregatedRideShareSetting aggregatedRideShare | isJust aggregatedRideShare]
          )
          [Se.Is BeamP.personId (Se.Eq $ getId personId)]
      else do
        safetySettings <- getDefaultSettings
        let merged =
              DSafety.SafetySettings
                { DSafety.aggregatedRideShareSetting = aggregatedRideShare <|> safetySettings.aggregatedRideShareSetting,
                  DSafety.autoCallDefaultContact = fromMaybe safetySettings.autoCallDefaultContact autoCallDefaultContact,
                  DSafety.enableOtpLessRide = enableOtpLessRide <|> safetySettings.enableOtpLessRide,
                  DSafety.enablePostRideSafetyCheck = fromMaybe safetySettings.enablePostRideSafetyCheck enablePostRideSafetyCheck,
                  DSafety.enableUnexpectedEventsCheck = fromMaybe safetySettings.enableUnexpectedEventsCheck enableUnexpectedEventsCheck,
                  DSafety.falseSafetyAlarmCount = safetySettings.falseSafetyAlarmCount,
                  DSafety.hasCompletedMockSafetyDrill = hasCompletedMockSafetyDrill <|> safetySettings.hasCompletedMockSafetyDrill,
                  DSafety.hasCompletedSafetySetup = fromMaybe safetySettings.hasCompletedSafetySetup hasCompletedSafetySetup,
                  DSafety.informPoliceSos = fromMaybe safetySettings.informPoliceSos informPoliceSos,
                  DSafety.nightSafetyChecks = fromMaybe safetySettings.nightSafetyChecks nightSafetyChecks,
                  DSafety.notifySafetyTeamForSafetyCheckFailure = fromMaybe safetySettings.notifySafetyTeamForSafetyCheckFailure notifySafetyTeamForSafetyCheckFailure,
                  DSafety.notifySosWithEmergencyContacts = fromMaybe safetySettings.notifySosWithEmergencyContacts notifySosWithEmergencyContacts,
                  DSafety.personId = safetySettings.personId,
                  DSafety.safetyCenterDisabledOnDate = safetySettings.safetyCenterDisabledOnDate,
                  DSafety.shakeToActivate = fromMaybe safetySettings.shakeToActivate shakeToActivate,
                  DSafety.updatedAt = now
                }
        createWithKV merged

-- | Update safety center blocking counter and optional disabled date.
updateSafetyCenterBlockingCounter ::
  (BeamFlow m r, MonadFlow m) =>
  Id SafetyCommon.Person ->
  Maybe Int ->
  Maybe UTCTime ->
  m ()
updateSafetyCenterBlockingCounter personId counter mbDate =
  updateWithKV
    ( [Se.Set BeamP.safetyCenterDisabledOnDate mbDate]
        <> [Se.Set BeamP.falseSafetyAlarmCount counter | isJust counter]
    )
    [Se.Is BeamP.personId (Se.Eq $ getId personId)]

mkSafetySettingsByPersonIdKey :: Text -> Text
mkSafetySettingsByPersonIdKey personId = "SafetySettings:PersonId:" <> personId
