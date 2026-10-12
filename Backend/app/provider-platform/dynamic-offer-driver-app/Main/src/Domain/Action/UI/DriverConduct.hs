module Domain.Action.UI.DriverConduct (getDriverConductCurrent) where

import qualified API.Types.UI.DriverConduct as API
import qualified Data.List as List
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Overlay as DOverlay
import qualified Domain.Types.Person
import qualified Environment
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.BehaviorTracker.ActiveConsequences as BAC
import qualified Lib.BehaviorTracker.Types as BTT
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified SharedLogic.BehaviourManagement.Conduct as Conduct
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import Tools.Error

-- | The one consequence the driver should see now (or none). Read-only: active
-- consequences recorded by the behaviour dispatcher, checked against
-- driver_information, highest priority wins, message from the CONDUCT_* overlays.
getDriverConductCurrent ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow API.DriverConductCurrentRes
  )
getDriverConductCurrent (mbPersonId, _merchantId, merchantOpCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- QDI.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  now <- getCurrentTime
  active <- BAC.readActive BTT.DRIVER personId.getId
  case Conduct.selectCurrent now (Conduct.reconcileWithDriverInfo now driverInfo active) of
    Nothing -> pure API.DriverConductCurrentRes {current = Nothing}
    Just consequence -> do
      transporterConfig <-
        getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
          >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      let keys = Conduct.messageKeyCandidates consequence
          languages = List.nub [fromMaybe ENGLISH person.language, ENGLISH]
      mbTemplate <- firstTemplate [(key, lang) | key <- keys, lang <- languages]
      when (isNothing mbTemplate) $
        logWarning $ "No conduct message template for driver " <> personId.getId <> "; tried overlay keys " <> show keys
      let render = Conduct.renderPlaceholders (Conduct.placeholderValues transporterConfig.timeDiffFromUtc consequence)
          fromTemplate field = render <$> (mbTemplate >>= field)
      pure
        API.DriverConductCurrentRes
          { current =
              Just
                API.CurrentConsequence
                  { consequenceType = consequence.consequenceType,
                    programme = consequence.programme,
                    appliedAt = consequence.appliedAt,
                    validTill = consequence.validTill,
                    title = fromTemplate (.title),
                    description = fromTemplate (.description),
                    imageUrl = mbTemplate >>= (.imageUrl),
                    okButtonText = fromTemplate (.okButtonText)
                  }
          }
  where
    -- Most specific key first; within a key, the driver's language before English.
    firstTemplate :: [(Text, Language)] -> Environment.Flow (Maybe DOverlay.Overlay)
    firstTemplate [] = pure Nothing
    firstTemplate ((key, lang) : rest) =
      CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory merchantOpCityId key lang Nothing Nothing Nothing
        >>= maybe (firstTemplate rest) (pure . Just)
