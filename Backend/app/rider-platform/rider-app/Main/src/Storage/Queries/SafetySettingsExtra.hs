module Storage.Queries.SafetySettingsExtra where

import Control.Applicative ((<|>))
import Domain.Types.Person
import qualified Domain.Types.SafetySettings as DSafety
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SafetySettings as BeamP
import Storage.Queries.OrphanInstances.SafetySettings ()
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
    enableOtpLessRide :: Maybe Bool,
    aggregatedRideShare :: Maybe RideShareOptions
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

emptyUpdateEmergencyInfo :: UpdateEmergencyInfo
emptyUpdateEmergencyInfo = UpdateEmergencyInfo Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- Extra code goes here --
upsert ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Kernel.Types.Id.Id Person ->
  UpdateEmergencyInfo ->
  m ()
upsert (Kernel.Types.Id.Id personId) UpdateEmergencyInfo {..} = Hedis.withLockRedis (mkSafetySettingsByPersonIdKey personId) 1 $ do
  now <- getCurrentTime
  res <- findOneWithKV [Se.And [Se.Is BeamP.personId $ Se.Eq personId]]
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
        [Se.Is BeamP.personId (Se.Eq personId)]
    else do
      person <- runInReplica $ QPerson.findById (Kernel.Types.Id.Id personId) >>= fromMaybeM (PersonNotFound personId)
      let enableUnexpectedEventsCheckValue = maybe (bool NEVER_SHARE SHARE_WITH_TIME_CONSTRAINTS person.nightSafetyChecks) identity enableUnexpectedEventsCheck
          safetySettings =
            DSafety.SafetySettings
              { autoCallDefaultContact = fromMaybe person.shareEmergencyContacts autoCallDefaultContact,
                enablePostRideSafetyCheck = fromMaybe NEVER_SHARE enablePostRideSafetyCheck,
                enableUnexpectedEventsCheck = enableUnexpectedEventsCheckValue,
                falseSafetyAlarmCount = person.falseSafetyAlarmCount,
                hasCompletedMockSafetyDrill = bool person.hasCompletedMockSafetyDrill hasCompletedMockSafetyDrill (isJust hasCompletedMockSafetyDrill),
                hasCompletedSafetySetup = fromMaybe person.hasCompletedSafetySetup hasCompletedSafetySetup,
                informPoliceSos = fromMaybe person.informPoliceSos informPoliceSos,
                nightSafetyChecks = fromMaybe person.nightSafetyChecks nightSafetyChecks,
                notifySafetyTeamForSafetyCheckFailure = fromMaybe False notifySafetyTeamForSafetyCheckFailure,
                notifySosWithEmergencyContacts = fromMaybe person.shareEmergencyContacts notifySosWithEmergencyContacts,
                personId = Kernel.Types.Id.Id personId,
                safetyCenterDisabledOnDate = person.safetyCenterDisabledOnDate,
                shakeToActivate = fromMaybe False shakeToActivate,
                updatedAt = now,
                enableOtpLessRide = enableOtpLessRide <|> person.enableOtpLessRide,
                aggregatedRideShareSetting = person.shareTripWithEmergencyContactOption
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
findSafetySettingsWithFallback personId mbPerson = Hedis.withLockRedisAndReturnValue (mkSafetySettingsByPersonIdKey personId.getId) 1 $ do
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
                aggregatedRideShareSetting = person.shareTripWithEmergencyContactOption,
                ..
              }
      _ <- createWithKV safetySettings
      return safetySettings

mkSafetySettingsByPersonIdKey :: Text -> Text
mkSafetySettingsByPersonIdKey personId = "SafetySettings:PersonId:" <> personId
