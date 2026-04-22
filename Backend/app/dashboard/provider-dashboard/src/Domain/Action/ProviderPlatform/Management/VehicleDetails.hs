module Domain.Action.ProviderPlatform.Management.VehicleDetails
  ( getVehicleDetailsVehicleModels
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.VehicleDetails
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getVehicleDetailsVehicleModels :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow [API.Types.ProviderPlatform.Management.VehicleDetails.VehicleMakeModelsItem])
getVehicleDetailsVehicleModels merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.vehicleDetailsDSL.getVehicleDetailsVehicleModels)
