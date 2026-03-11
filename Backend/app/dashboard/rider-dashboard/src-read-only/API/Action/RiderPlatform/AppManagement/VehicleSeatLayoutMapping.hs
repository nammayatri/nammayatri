{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.VehicleSeatLayoutMapping
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping
import qualified Data.Text
import qualified Domain.Action.RiderPlatform.AppManagement.VehicleSeatLayoutMapping
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("vehicleSeatLayoutMapping" :> (ListVehicleSeatLayoutMapping :<|> UpsertVehicleSeatLayoutMapping :<|> DeleteVehicleSeatLayoutMapping))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = listVehicleSeatLayoutMapping merchantId city :<|> upsertVehicleSeatLayoutMapping merchantId city :<|> deleteVehicleSeatLayoutMapping merchantId city

type ListVehicleSeatLayoutMapping =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.VEHICLE_SEAT_LAYOUT_MAPPING) / ('API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.LIST_VEHICLE_SEAT_LAYOUT_MAPPING))
      :> API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.ListVehicleSeatLayoutMapping
  )

type UpsertVehicleSeatLayoutMapping =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.VEHICLE_SEAT_LAYOUT_MAPPING) / ('API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.UPSERT_VEHICLE_SEAT_LAYOUT_MAPPING))
      :> API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.UpsertVehicleSeatLayoutMapping
  )

type DeleteVehicleSeatLayoutMapping =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.VEHICLE_SEAT_LAYOUT_MAPPING) / ('API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.DELETE_VEHICLE_SEAT_LAYOUT_MAPPING))
      :> API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.DeleteVehicleSeatLayoutMapping
  )

listVehicleSeatLayoutMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Data.Text.Text -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.VehicleSeatLayoutMappingItem])
listVehicleSeatLayoutMapping merchantShortId opCity apiTokenInfo limit offset gtfsId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.VehicleSeatLayoutMapping.listVehicleSeatLayoutMapping merchantShortId opCity apiTokenInfo limit offset gtfsId

upsertVehicleSeatLayoutMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.VehicleSeatLayoutMapping.VehicleSeatLayoutMappingUpsertReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
upsertVehicleSeatLayoutMapping merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.VehicleSeatLayoutMapping.upsertVehicleSeatLayoutMapping merchantShortId opCity apiTokenInfo req

deleteVehicleSeatLayoutMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> Data.Text.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteVehicleSeatLayoutMapping merchantShortId opCity apiTokenInfo vehicleNo gtfsId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.VehicleSeatLayoutMapping.deleteVehicleSeatLayoutMapping merchantShortId opCity apiTokenInfo vehicleNo gtfsId
