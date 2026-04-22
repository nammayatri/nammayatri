module Domain.Action.Dashboard.Management.VehicleDetails
  ( getVehicleDetailsVehicleModels
  )
where

import qualified API.Types.ProviderPlatform.Management.VehicleDetails
import qualified Domain.Action.Common.VehicleDetails as CVehicleDetails
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

getVehicleDetailsVehicleModels :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow [API.Types.ProviderPlatform.Management.VehicleDetails.VehicleMakeModelsItem])
getVehicleDetailsVehicleModels merchantShortId opCity = do
  merchantOpCity <- resolveMerchantOpCity opCity
  vehicleDetails <- CVehicleDetails.getFilteredVehicleDetails merchantOpCity.country merchantOpCity.id
  pure $
    CVehicleDetails.toMakeModelTuples vehicleDetails
      <&> \(make, models) ->
        API.Types.ProviderPlatform.Management.VehicleDetails.VehicleMakeModelsItem {make, models}
  where
    resolveMerchantOpCity city' =
      CQMOC.findByMerchantShortIdAndCity merchantShortId city'
        >>= fromMaybeM
          (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city')
