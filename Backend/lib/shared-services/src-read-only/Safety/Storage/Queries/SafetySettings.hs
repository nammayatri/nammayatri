{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Safety.Storage.Queries.SafetySettings (module Safety.Storage.Queries.SafetySettings, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Safety.Storage.Queries.SafetySettingsExtra as ReExport
import qualified Safety.Domain.Types.SafetySettings
import qualified Safety.Storage.Beam.SafetySettings as Beam
import qualified Kernel.Prelude
import qualified Safety.Domain.Types.Common
import qualified Kernel.Types.Id
import qualified Safety.Storage.BeamFlow
import qualified Sequelize as Se



create :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Safety.Domain.Types.SafetySettings.SafetySettings -> m ())
create = createWithKV
createMany :: (Safety.Storage.BeamFlow.BeamFlow m r) => ([Safety.Domain.Types.SafetySettings.SafetySettings] -> m ())
createMany = traverse_ create
findByPersonId :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Safety.Domain.Types.Common.Person -> m (Maybe Safety.Domain.Types.SafetySettings.SafetySettings))
findByPersonId personId = do findOneWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]
updateEmergencyContactStatus :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Types.Id.Id Safety.Domain.Types.Common.Person -> m ())
updateEmergencyContactStatus autoCallDefaultContact notifySosWithEmergencyContacts personId = do {_now <- getCurrentTime;
                                                                                                  updateWithKV [Se.Set Beam.autoCallDefaultContact autoCallDefaultContact,
                                                                                                                Se.Set Beam.notifySosWithEmergencyContacts notifySosWithEmergencyContacts,
                                                                                                                Se.Set Beam.updatedAt _now] [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]}
updateMockSafetyDrillStatus :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Safety.Domain.Types.Common.Person -> m ())
updateMockSafetyDrillStatus hasCompletedMockSafetyDrill personId = do {_now <- getCurrentTime;
                                                                       updateWithKV [Se.Set Beam.hasCompletedMockSafetyDrill hasCompletedMockSafetyDrill, Se.Set Beam.updatedAt _now] [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]}
findByPrimaryKey :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Safety.Domain.Types.Common.Person -> m (Maybe Safety.Domain.Types.SafetySettings.SafetySettings))
findByPrimaryKey personId = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]
updateByPrimaryKey :: (Safety.Storage.BeamFlow.BeamFlow m r) => (Safety.Domain.Types.SafetySettings.SafetySettings -> m ())
updateByPrimaryKey (Safety.Domain.Types.SafetySettings.SafetySettings {..}) = do {_now <- getCurrentTime;
                                                                                  updateWithKV [Se.Set Beam.aggregatedRideShareSetting aggregatedRideShareSetting,
                                                                                                Se.Set Beam.autoCallDefaultContact autoCallDefaultContact,
                                                                                                Se.Set Beam.enableOtpLessRide enableOtpLessRide,
                                                                                                Se.Set Beam.enablePostRideSafetyCheck enablePostRideSafetyCheck,
                                                                                                Se.Set Beam.enableUnexpectedEventsCheck enableUnexpectedEventsCheck,
                                                                                                Se.Set Beam.falseSafetyAlarmCount ((Kernel.Prelude.Just falseSafetyAlarmCount)),
                                                                                                Se.Set Beam.hasCompletedMockSafetyDrill hasCompletedMockSafetyDrill,
                                                                                                Se.Set Beam.hasCompletedSafetySetup hasCompletedSafetySetup,
                                                                                                Se.Set Beam.informPoliceSos informPoliceSos,
                                                                                                Se.Set Beam.nightSafetyChecks nightSafetyChecks,
                                                                                                Se.Set Beam.notifySafetyTeamForSafetyCheckFailure notifySafetyTeamForSafetyCheckFailure,
                                                                                                Se.Set Beam.notifySosWithEmergencyContacts notifySosWithEmergencyContacts,
                                                                                                Se.Set Beam.safetyCenterDisabledOnDate safetyCenterDisabledOnDate,
                                                                                                Se.Set Beam.shakeToActivate shakeToActivate,
                                                                                                Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]}



