{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.VehicleInfo
  ( API.Types.ProviderPlatform.Management.VehicleInfo.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.VehicleInfo.API)
handler merchantId city = getVehicleInfoList merchantId city :<|> postVehicleInfoUpdate merchantId city

getVehicleInfoList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.VehicleInfo.VehicleExtraInformation)
getVehicleInfoList a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.VehicleInfo.getVehicleInfoList a3 a2 a1

postVehicleInfoUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.VehicleInfo.UpdateVehicleInfoReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postVehicleInfoUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.VehicleInfo.postVehicleInfoUpdate a3 a2 a1
