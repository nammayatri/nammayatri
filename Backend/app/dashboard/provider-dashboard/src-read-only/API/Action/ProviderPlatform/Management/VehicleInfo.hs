{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.VehicleInfo
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.VehicleInfo
import qualified Domain.Action.ProviderPlatform.Management.VehicleInfo
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("vehicleInfo" :> (GetVehicleInfoList :<|> PostVehicleInfoUpdate))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getVehicleInfoList merchantId city :<|> postVehicleInfoUpdate merchantId city

type GetVehicleInfoList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.VEHICLE_INFO / 'API.Types.ProviderPlatform.Management.VehicleInfo.GET_VEHICLE_INFO_LIST)
      :> API.Types.ProviderPlatform.Management.VehicleInfo.GetVehicleInfoList
  )

type PostVehicleInfoUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.VEHICLE_INFO / 'API.Types.ProviderPlatform.Management.VehicleInfo.POST_VEHICLE_INFO_UPDATE)
      :> API.Types.ProviderPlatform.Management.VehicleInfo.PostVehicleInfoUpdate
  )

getVehicleInfoList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.VehicleInfo.VehicleExtraInformation)
getVehicleInfoList merchantShortId opCity apiTokenInfo rcNo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.VehicleInfo.getVehicleInfoList merchantShortId opCity apiTokenInfo rcNo

postVehicleInfoUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.VehicleInfo.UpdateVehicleInfoReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postVehicleInfoUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.VehicleInfo.postVehicleInfoUpdate merchantShortId opCity apiTokenInfo req
