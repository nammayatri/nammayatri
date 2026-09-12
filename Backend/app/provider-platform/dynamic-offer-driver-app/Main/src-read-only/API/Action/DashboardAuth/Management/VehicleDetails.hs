{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.VehicleDetails
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.VehicleDetails
import qualified Domain.Action.Dashboard.Management.VehicleDetails
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("vehicleDetails" :> GetVehicleDetailsVehicleModels)

type GetVehicleDetailsVehicleModels =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/VEHICLE_DETAILS/GET_VEHICLE_DETAILS_VEHICLE_MODELS"
      :> API.Types.ProviderPlatform.Management.VehicleDetails.GetVehicleDetailsVehicleModels
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getVehicleDetailsVehicleModels merchantId city

getVehicleDetailsVehicleModels :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.VehicleDetails.VehicleMakeModelsItem])
getVehicleDetailsVehicleModels a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.VehicleDetails.getVehicleDetailsVehicleModels a3 a2
