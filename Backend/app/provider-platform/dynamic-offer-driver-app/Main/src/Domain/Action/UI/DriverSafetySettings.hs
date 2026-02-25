{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.DriverSafetySettings
  ( getDriverGetSafetySettings,
    putDriverUpdateSafetySettings,
  )
where

import qualified API.Types.UI.DriverSafetySettings as API
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Safety.Domain.Types.Common as SafetyCommon
import qualified Safety.Domain.Types.SafetySettings as DSafety
import qualified Safety.Storage.Queries.SafetySettings as QSafetySettings
import Servant hiding (throwError)
import qualified Storage.Queries.PersonDefaultEmergencyNumber as QPDEN
import qualified Storage.Queries.SafetySettingsExtra as QSafetyExtra
import Tools.Auth
import Tools.Error

-- | Derive aggregatedRideShareSetting from emergency contacts (first contact's option), same as rider.
deriveAggregatedRideShareSetting :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Kernel.Prelude.Maybe SafetyCommon.RideShareOptions)
deriveAggregatedRideShareSetting personId = do
  personENList <- QPDEN.findAllByPersonId personId
  return $ Kernel.Prelude.listToMaybe personENList >>= (.shareTripWithEmergencyContactOption)

getDriverGetSafetySettings ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Person.Person ->
    Environment.Flow API.GetDriverSafetySettingsRes
  )
getDriverGetSafetySettings (mbPersonId, _, _) personId = do
  authPersonId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  unless (authPersonId == personId) $ throwError $ InvalidRequest "PersonId mismatch"
  safetySettings <- QSafetyExtra.findSafetySettingsWithFallback personId
  aggregatedRideShareSetting <- deriveAggregatedRideShareSetting personId
  return $
    (toGetRes personId safetySettings)
      { API.aggregatedRideShareSetting = aggregatedRideShareSetting
      }

putDriverUpdateSafetySettings ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.UpdateDriverSafetySettingsReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
putDriverUpdateSafetySettings (mbPersonId, _, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  safetySettings <- QSafetyExtra.findSafetySettingsWithFallback personId
  let updated = applyUpdateReq safetySettings req
  QSafetySettings.updateByPrimaryKey updated
  pure Kernel.Types.APISuccess.Success

toGetRes :: Id Domain.Types.Person.Person -> DSafety.SafetySettings -> API.GetDriverSafetySettingsRes
toGetRes personId ss =
  API.GetDriverSafetySettingsRes
    { aggregatedRideShareSetting = ss.aggregatedRideShareSetting,
      autoCallDefaultContact = ss.autoCallDefaultContact,
      enableOtpLessRide = ss.enableOtpLessRide,
      enablePostRideSafetyCheck = ss.enablePostRideSafetyCheck,
      enableUnexpectedEventsCheck = ss.enableUnexpectedEventsCheck,
      falseSafetyAlarmCount = ss.falseSafetyAlarmCount,
      hasCompletedMockSafetyDrill = ss.hasCompletedMockSafetyDrill,
      hasCompletedSafetySetup = ss.hasCompletedSafetySetup,
      informPoliceSos = ss.informPoliceSos,
      nightSafetyChecks = ss.nightSafetyChecks,
      notifySafetyTeamForSafetyCheckFailure = ss.notifySafetyTeamForSafetyCheckFailure,
      notifySosWithEmergencyContacts = ss.notifySosWithEmergencyContacts,
      personId = personId,
      safetyCenterDisabledOnDate = ss.safetyCenterDisabledOnDate,
      shakeToActivate = ss.shakeToActivate
    }

applyUpdateReq :: DSafety.SafetySettings -> API.UpdateDriverSafetySettingsReq -> DSafety.SafetySettings
applyUpdateReq ss req =
  ss
    { -- aggregatedRideShareSetting is derived from emergency contacts (same as rider), not set via this API
      DSafety.autoCallDefaultContact = fromMaybe ss.autoCallDefaultContact req.autoCallDefaultContact,
      DSafety.enableOtpLessRide = req.enableOtpLessRide <|> ss.enableOtpLessRide,
      DSafety.enablePostRideSafetyCheck = fromMaybe ss.enablePostRideSafetyCheck req.enablePostRideSafetyCheck,
      DSafety.enableUnexpectedEventsCheck = fromMaybe ss.enableUnexpectedEventsCheck req.enableUnexpectedEventsCheck,
      DSafety.hasCompletedMockSafetyDrill = req.hasCompletedMockSafetyDrill <|> ss.hasCompletedMockSafetyDrill,
      DSafety.hasCompletedSafetySetup = fromMaybe ss.hasCompletedSafetySetup req.hasCompletedSafetySetup,
      DSafety.informPoliceSos = fromMaybe ss.informPoliceSos req.informPoliceSos,
      DSafety.nightSafetyChecks = fromMaybe ss.nightSafetyChecks req.nightSafetyChecks,
      DSafety.notifySafetyTeamForSafetyCheckFailure = fromMaybe ss.notifySafetyTeamForSafetyCheckFailure req.notifySafetyTeamForSafetyCheckFailure,
      DSafety.notifySosWithEmergencyContacts = fromMaybe ss.notifySosWithEmergencyContacts req.notifySosWithEmergencyContacts,
      DSafety.shakeToActivate = fromMaybe ss.shakeToActivate req.shakeToActivate
    }
