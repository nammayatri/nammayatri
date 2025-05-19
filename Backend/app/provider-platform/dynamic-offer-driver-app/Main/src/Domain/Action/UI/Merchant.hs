module Domain.Action.UI.Merchant where

import qualified API.Types.UI.Merchant
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error.Throwing (fromMaybeM)
import qualified Storage.Cac.TransporterConfig as CCT

makeMerchantAPIEntity :: Merchant -> MerchantAPIEntity
makeMerchantAPIEntity Merchant {..} =
  MerchantAPIEntity
    { contactNumber = fromMaybe "Unknown" $ mobileCountryCode <> mobileNumber,
      ..
    }

getCityConfigs ::
  ( Kernel.Prelude.Maybe (Id Domain.Types.Person.Person),
    Id Domain.Types.Merchant.Merchant,
    Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Environment.Flow API.Types.UI.Merchant.CityConfigs
getCityConfigs (_, _, merchantOpCityId) = do
  transporterConfig <- CCT.findByMerchantOpCityId (cast merchantOpCityId) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  pure $
    API.Types.UI.Merchant.CityConfigs
      { localPoliceNumbers = fromMaybe [] transporterConfig.localPoliceNumbers,
        localAmbulanceNumbers = fromMaybe [] transporterConfig.localAmbulanceNumbers,
        safetyTeamNumbers = fromMaybe [] transporterConfig.safetyTeamNumbers
      }
