{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SafetySettingsExtra where

import Control.Applicative ((<|>))
import Domain.Types.Person
import qualified Domain.Types.SafetySettings as DSafety
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SafetySettings as BeamP
import Storage.Queries.OrphanInstances.SafetySettings
import qualified Storage.Queries.Person as QPerson

data UpdateEmergencyInfo = UpdateEmergencyInfo
  { autoCallDefaultContact :: Maybe Bool,
    enablePostRideSafetyCheck :: Maybe RideShareOptions,
    enableUnexpectedEventsCheck :: Maybe RideShareOptions,
    hasCompletedMockSafetyDrill :: Maybe Bool,
    hasCompletedSafetySetup :: Maybe Bool,
    informPoliceSos :: Maybe Bool,
    nightSafetyChecks :: Maybe Bool,
    notifySafetyTeamForSafetyCheckFailure :: Maybe Bool,
    notifySosWithEmergencyContacts :: Maybe Bool,
    shakeToActivate :: Maybe Bool,
    safetyCenterDisabledOnDate :: Maybe UTCTime,
    enableOtpLessRide :: Maybe Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- Extra code goes here --
upsert ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Kernel.Types.Id.Id Person ->
  UpdateEmergencyInfo ->
  m ()
upsert (Kernel.Types.Id.Id personId) UpdateEmergencyInfo {..} = do
  now <- getCurrentTime
  res <- findOneWithKV [Se.And [Se.Is BeamP.personId $ Se.Eq personId]]
  if isJust res
    then
      updateWithKV
        ( [Se.Set BeamP.updatedAt now]
            <> [Se.Set BeamP.autoCallDefaultContact (fromJust autoCallDefaultContact) | isJust autoCallDefaultContact]
            <> [Se.Set BeamP.enablePostRideSafetyCheck (fromJust enablePostRideSafetyCheck) | isJust enablePostRideSafetyCheck]
            <> [Se.Set BeamP.enableUnexpectedEventsCheck (fromJust enableUnexpectedEventsCheck) | isJust enableUnexpectedEventsCheck]
            <> [Se.Set BeamP.hasCompletedMockSafetyDrill hasCompletedMockSafetyDrill]
            <> [Se.Set BeamP.hasCompletedSafetySetup (fromJust hasCompletedSafetySetup) | isJust hasCompletedSafetySetup]
            <> [Se.Set BeamP.informPoliceSos (fromJust informPoliceSos) | isJust informPoliceSos]
            <> [Se.Set BeamP.nightSafetyChecks (fromJust nightSafetyChecks) | isJust nightSafetyChecks]
            <> [Se.Set BeamP.notifySafetyTeamForSafetyCheckFailure (fromJust notifySafetyTeamForSafetyCheckFailure) | isJust notifySafetyTeamForSafetyCheckFailure]
            <> [Se.Set BeamP.notifySosWithEmergencyContacts (fromJust notifySosWithEmergencyContacts) | isJust notifySosWithEmergencyContacts]
            <> [Se.Set BeamP.shakeToActivate (fromJust shakeToActivate) | isJust shakeToActivate]
            <> [Se.Set BeamP.safetyCenterDisabledOnDate safetyCenterDisabledOnDate]
            <> [Se.Set BeamP.enableOtpLessRide enableOtpLessRide | isJust enableOtpLessRide]
        )
        [Se.Is BeamP.personId (Se.Eq personId)]
    else do
      person <- runInReplica $ QPerson.findById (Kernel.Types.Id.Id personId) >>= fromMaybeM (PersonNotFound personId)
      let safetySettings =
            DSafety.SafetySettings
              { autoCallDefaultContact = fromMaybe person.shareEmergencyContacts autoCallDefaultContact,
                enablePostRideSafetyCheck = fromMaybe NEVER_SHARE enablePostRideSafetyCheck,
                enableUnexpectedEventsCheck = fromMaybe NEVER_SHARE enableUnexpectedEventsCheck,
                falseSafetyAlarmCount = person.falseSafetyAlarmCount,
                hasCompletedMockSafetyDrill = bool person.hasCompletedMockSafetyDrill hasCompletedMockSafetyDrill (isJust hasCompletedMockSafetyDrill),
                hasCompletedSafetySetup = fromMaybe person.hasCompletedSafetySetup hasCompletedSafetySetup,
                informPoliceSos = fromMaybe person.informPoliceSos informPoliceSos,
                nightSafetyChecks = fromMaybe person.nightSafetyChecks nightSafetyChecks,
                notifySafetyTeamForSafetyCheckFailure = fromMaybe False notifySafetyTeamForSafetyCheckFailure,
                notifySosWithEmergencyContacts = fromMaybe person.shareEmergencyContacts notifySosWithEmergencyContacts,
                personId = Kernel.Types.Id.Id personId,
                safetyCenterDisabledOnDate = bool person.safetyCenterDisabledOnDate safetyCenterDisabledOnDate (isJust safetyCenterDisabledOnDate),
                shakeToActivate = fromMaybe False shakeToActivate,
                updatedAt = now,
                enableOtpLessRide = enableOtpLessRide <|> person.enableOtpLessRide
              }
      createWithKV safetySettings

updateSafetyCenterBlockingCounter :: (MonadFlow m, EsqDBFlow m r) => Kernel.Types.Id.Id Person -> Maybe Int -> Maybe UTCTime -> m ()
updateSafetyCenterBlockingCounter personId counter mbDate = do
  now <- getCurrentTime
  updateWithKV
    ( [ Se.Set BeamP.updatedAt now,
        Se.Set BeamP.safetyCenterDisabledOnDate mbDate
      ]
        <> [Se.Set BeamP.falseSafetyAlarmCount counter | isJust counter]
    )
    [Se.Is BeamP.personId (Se.Eq personId.getId)]

findSafetySettingsWithFallback :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Kernel.Types.Id.Id Person -> Maybe Person -> m DSafety.SafetySettings
findSafetySettingsWithFallback personId mbPerson = do
  now <- getCurrentTime
  res <- findOneWithKV [Se.And [Se.Is BeamP.personId $ Se.Eq personId.getId]]
  case res of
    Just safetySettings -> return safetySettings
    Nothing -> do
      person <- maybe (runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)) return mbPerson
      let safetySettings =
            DSafety.SafetySettings
              { autoCallDefaultContact = person.shareEmergencyContacts,
                enablePostRideSafetyCheck = NEVER_SHARE,
                enableUnexpectedEventsCheck = NEVER_SHARE,
                falseSafetyAlarmCount = person.falseSafetyAlarmCount,
                hasCompletedMockSafetyDrill = person.hasCompletedMockSafetyDrill,
                hasCompletedSafetySetup = person.hasCompletedSafetySetup,
                informPoliceSos = person.informPoliceSos,
                nightSafetyChecks = person.nightSafetyChecks,
                notifySafetyTeamForSafetyCheckFailure = False,
                notifySosWithEmergencyContacts = person.shareEmergencyContacts,
                safetyCenterDisabledOnDate = person.safetyCenterDisabledOnDate,
                shakeToActivate = False,
                updatedAt = now,
                enableOtpLessRide = person.enableOtpLessRide,
                ..
              }
      _ <- createWithKV safetySettings
      return safetySettings
