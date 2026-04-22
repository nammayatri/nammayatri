{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.VehicleDetails
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.VehicleDetails
import qualified Domain.Action.ProviderPlatform.Management.VehicleDetails
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("vehicleDetails" :> GetVehicleDetailsVehicleModels)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getVehicleDetailsVehicleModels merchantId city

type GetVehicleDetailsVehicleModels =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.VEHICLE_DETAILS) / ('API.Types.ProviderPlatform.Management.VehicleDetails.GET_VEHICLE_DETAILS_VEHICLE_MODELS))
      :> API.Types.ProviderPlatform.Management.VehicleDetails.GetVehicleDetailsVehicleModels
  )

getVehicleDetailsVehicleModels :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.VehicleDetails.VehicleMakeModelsItem])
getVehicleDetailsVehicleModels merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.VehicleDetails.getVehicleDetailsVehicleModels merchantShortId opCity apiTokenInfo
