module Domain.Action.UI.DriverVoiceUpdate where

import qualified API.Types.UI.DriverVoiceUpdate as APITypes
import qualified Domain.Types.DriverVoiceSettings as DDVS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.VoiceNotification as VN
import qualified Storage.Queries.DriverVoiceSettings as QDVS

-- | Get the driver's voice notification settings
getDriverVoiceSettings ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Flow APITypes.GetDriverVoiceSettingsRes
  )
getDriverVoiceSettings (mbPersonId, _merchantId, merchantOpCityId) = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  mbSettings <- QDVS.findByDriverId driverId
  case mbSettings of
    Just settings ->
      pure
        APITypes.GetDriverVoiceSettingsRes
          { isEnabled = settings.isEnabled,
            language = settings.language,
            volumeLevel = settings.volumeLevel,
            rideRequestEnabled = settings.rideRequestEnabled,
            navigationEnabled = settings.navigationEnabled,
            earningsEnabled = settings.earningsEnabled,
            safetyAlertsEnabled = settings.safetyAlertsEnabled,
            contextualUpdatesEnabled = settings.contextualUpdatesEnabled
          }
    Nothing ->
      pure
        APITypes.GetDriverVoiceSettingsRes
          { isEnabled = False,
            language = "en",
            volumeLevel = 80,
            rideRequestEnabled = True,
            navigationEnabled = False,
            earningsEnabled = True,
            safetyAlertsEnabled = True,
            contextualUpdatesEnabled = False
          }

-- | Update the driver's voice notification settings
putDriverVoiceSettings ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    APITypes.UpdateDriverVoiceSettingsReq ->
    Flow APISuccess
  )
putDriverVoiceSettings (mbPersonId, merchantId, merchantOpCityId) req = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  mbExisting <- QDVS.findByDriverId driverId
  now <- getCurrentTime
  case mbExisting of
    Just existing -> do
      let updated =
            existing
              { DDVS.isEnabled = fromMaybe existing.isEnabled req.isEnabled,
                DDVS.language = fromMaybe existing.language req.language,
                DDVS.volumeLevel = fromMaybe existing.volumeLevel req.volumeLevel,
                DDVS.rideRequestEnabled = fromMaybe existing.rideRequestEnabled req.rideRequestEnabled,
                DDVS.navigationEnabled = fromMaybe existing.navigationEnabled req.navigationEnabled,
                DDVS.earningsEnabled = fromMaybe existing.earningsEnabled req.earningsEnabled,
                DDVS.safetyAlertsEnabled = fromMaybe existing.safetyAlertsEnabled req.safetyAlertsEnabled,
                DDVS.contextualUpdatesEnabled = fromMaybe existing.contextualUpdatesEnabled req.contextualUpdatesEnabled,
                DDVS.updatedAt = now
              }
      QDVS.updateByPrimaryKey updated
    Nothing -> do
      newId <- generateGUID
      let newSettings =
            DDVS.DriverVoiceSettings
              { driverId = driverId,
                isEnabled = fromMaybe False req.isEnabled,
                language = fromMaybe "en" req.language,
                volumeLevel = fromMaybe 80 req.volumeLevel,
                rideRequestEnabled = fromMaybe True req.rideRequestEnabled,
                navigationEnabled = fromMaybe False req.navigationEnabled,
                earningsEnabled = fromMaybe True req.earningsEnabled,
                safetyAlertsEnabled = fromMaybe True req.safetyAlertsEnabled,
                contextualUpdatesEnabled = fromMaybe False req.contextualUpdatesEnabled,
                merchantOperatingCityId = merchantOpCityId,
                merchantId = merchantId,
                createdAt = now,
                updatedAt = now
              }
      QDVS.create newSettings
  pure Success

-- | Generate a voice update audio clip on demand
postDriverVoiceUpdateGenerate ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    APITypes.VoiceUpdateGenerateReq ->
    Flow APITypes.VoiceUpdateGenerateRes
  )
postDriverVoiceUpdateGenerate (mbPersonId, merchantId, merchantOpCityId) req = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  mbSettings <- QDVS.findByDriverId driverId
  let lang = maybe "en" (.language) mbSettings
  VN.generateVoiceNotification merchantId merchantOpCityId req.updateType lang req.context
