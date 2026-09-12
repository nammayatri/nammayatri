{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.VehicleInfo
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.VehicleInfo
import qualified Domain.Action.Dashboard.Management.VehicleInfo
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("vehicleInfo" :> (GetVehicleInfoList :<|> PostVehicleInfoUpdate))

type GetVehicleInfoList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/VEHICLE_INFO/GET_VEHICLE_INFO_LIST"
      :> API.Types.ProviderPlatform.Management.VehicleInfo.GetVehicleInfoList
  )

type PostVehicleInfoUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/VEHICLE_INFO/POST_VEHICLE_INFO_UPDATE"
      :> API.Types.ProviderPlatform.Management.VehicleInfo.PostVehicleInfoUpdate
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getVehicleInfoList merchantId city :<|> postVehicleInfoUpdate merchantId city

getVehicleInfoList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.VehicleInfo.VehicleExtraInformation)
getVehicleInfoList a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.VehicleInfo.getVehicleInfoList a4 a3 a1

postVehicleInfoUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.VehicleInfo.UpdateVehicleInfoReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postVehicleInfoUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.VehicleInfo.postVehicleInfoUpdate a4 a3 a1
