{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.AppManagement.VehicleSeatLayoutMapping 
( API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.AppManagement.VehicleSeatLayoutMapping
import qualified Kernel.Prelude
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Data.Text
import qualified API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping
import qualified Kernel.Types.APISuccess



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.API)
handler merchantId city = listVehicleSeatLayoutMapping merchantId city :<|> upsertVehicleSeatLayoutMapping merchantId city :<|> deleteVehicleSeatLayoutMapping merchantId city
listVehicleSeatLayoutMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Data.Text.Text -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.VehicleSeatLayoutMappingItem])
listVehicleSeatLayoutMapping a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.VehicleSeatLayoutMapping.listVehicleSeatLayoutMapping a5 a4 a3 a2 a1
upsertVehicleSeatLayoutMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.VehicleSeatLayoutMappingUpsertReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
upsertVehicleSeatLayoutMapping a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.VehicleSeatLayoutMapping.upsertVehicleSeatLayoutMapping a3 a2 a1
deleteVehicleSeatLayoutMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> Data.Text.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteVehicleSeatLayoutMapping a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.VehicleSeatLayoutMapping.deleteVehicleSeatLayoutMapping a4 a3 a2 a1



