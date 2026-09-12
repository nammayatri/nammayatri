{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.VehicleSeatLayoutMapping
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping
import qualified Data.Text
import qualified Domain.Action.Dashboard.AppManagement.VehicleSeatLayoutMapping
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

type API = ("vehicleSeatLayoutMapping" :> (ListVehicleSeatLayoutMapping :<|> UpsertVehicleSeatLayoutMapping :<|> DeleteVehicleSeatLayoutMapping))

type ListVehicleSeatLayoutMapping =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/VEHICLE_SEAT_LAYOUT_MAPPING/LIST_VEHICLE_SEAT_LAYOUT_MAPPING"
      :> API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.ListVehicleSeatLayoutMapping
  )

type UpsertVehicleSeatLayoutMapping =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/VEHICLE_SEAT_LAYOUT_MAPPING/UPSERT_VEHICLE_SEAT_LAYOUT_MAPPING"
      :> API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.UpsertVehicleSeatLayoutMapping
  )

type DeleteVehicleSeatLayoutMapping =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/VEHICLE_SEAT_LAYOUT_MAPPING/DELETE_VEHICLE_SEAT_LAYOUT_MAPPING"
      :> API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.DeleteVehicleSeatLayoutMapping
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = listVehicleSeatLayoutMapping merchantId city :<|> upsertVehicleSeatLayoutMapping merchantId city :<|> deleteVehicleSeatLayoutMapping merchantId city

listVehicleSeatLayoutMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Data.Text.Text -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.VehicleSeatLayoutMappingItem])
listVehicleSeatLayoutMapping a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.VehicleSeatLayoutMapping.listVehicleSeatLayoutMapping a6 a5 a3 a2 a1

upsertVehicleSeatLayoutMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.VehicleSeatLayoutMappingUpsertReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
upsertVehicleSeatLayoutMapping a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.VehicleSeatLayoutMapping.upsertVehicleSeatLayoutMapping a4 a3 a1

deleteVehicleSeatLayoutMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Data.Text.Text -> Data.Text.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteVehicleSeatLayoutMapping a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.VehicleSeatLayoutMapping.deleteVehicleSeatLayoutMapping a5 a4 a2 a1
