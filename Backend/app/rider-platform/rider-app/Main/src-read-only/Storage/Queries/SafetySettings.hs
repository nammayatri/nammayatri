{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SafetySettings (module Storage.Queries.SafetySettings, module ReExport) where

import qualified Domain.Types.Person
import qualified Domain.Types.SafetySettings
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SafetySettings as Beam
import Storage.Queries.SafetySettingsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SafetySettings.SafetySettings -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SafetySettings.SafetySettings] -> m ())
createMany = traverse_ create

updateEmergencyContactStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateEmergencyContactStatus autoCallDefaultContact notifySosWithEmergencyContacts personId = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.autoCallDefaultContact autoCallDefaultContact,
      Se.Set Beam.notifySosWithEmergencyContacts notifySosWithEmergencyContacts,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateMockSafetyDrillStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateMockSafetyDrillStatus hasCompletedMockSafetyDrill personId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.hasCompletedMockSafetyDrill hasCompletedMockSafetyDrill, Se.Set Beam.updatedAt _now] [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.SafetySettings.SafetySettings))
findByPrimaryKey personId = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SafetySettings.SafetySettings -> m ())
updateByPrimaryKey (Domain.Types.SafetySettings.SafetySettings {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.aggregatedRideShareSetting aggregatedRideShareSetting,
      Se.Set Beam.autoCallDefaultContact autoCallDefaultContact,
      Se.Set Beam.enableOtpLessRide enableOtpLessRide,
      Se.Set Beam.enablePostRideSafetyCheck enablePostRideSafetyCheck,
      Se.Set Beam.enableUnexpectedEventsCheck enableUnexpectedEventsCheck,
      Se.Set Beam.falseSafetyAlarmCount (Just falseSafetyAlarmCount),
      Se.Set Beam.hasCompletedMockSafetyDrill hasCompletedMockSafetyDrill,
      Se.Set Beam.hasCompletedSafetySetup hasCompletedSafetySetup,
      Se.Set Beam.informPoliceSos informPoliceSos,
      Se.Set Beam.nightSafetyChecks nightSafetyChecks,
      Se.Set Beam.notifySafetyTeamForSafetyCheckFailure notifySafetyTeamForSafetyCheckFailure,
      Se.Set Beam.notifySosWithEmergencyContacts notifySosWithEmergencyContacts,
      Se.Set Beam.safetyCenterDisabledOnDate safetyCenterDisabledOnDate,
      Se.Set Beam.shakeToActivate shakeToActivate,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]
