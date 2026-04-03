{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.Management.VehicleInfo 
( API.Types.ProviderPlatform.Management.VehicleInfo.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.Management.VehicleInfo
import qualified Kernel.Prelude
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified API.Types.ProviderPlatform.Management.VehicleInfo
import qualified Kernel.Types.APISuccess



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.VehicleInfo.API)
handler merchantId city = getVehicleInfoList merchantId city :<|> postVehicleInfoUpdate merchantId city
getVehicleInfoList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.VehicleInfo.VehicleExtraInformation)
getVehicleInfoList a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.VehicleInfo.getVehicleInfoList a3 a2 a1
postVehicleInfoUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.VehicleInfo.UpdateVehicleInfoReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postVehicleInfoUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.VehicleInfo.postVehicleInfoUpdate a3 a2 a1



