{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.StopRouteDetails
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.StopRouteDetails
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Domain.Action.Dashboard.AppManagement.StopRouteDetails
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.RouteStopMapping
import qualified "this" Domain.Types.Station
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("stopRouteDetails" :> (StopRouteDetailsGetStops :<|> StopRouteDetailsGetStop :<|> StopRouteDetailsGetRouteStopMappingByStop :<|> StopRouteDetailsGetRouteStopMappingByRoute))

type StopRouteDetailsGetStops =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/STOP_ROUTE_DETAILS/STOP_ROUTE_DETAILS_GET_STOPS"
      :> API.Types.Dashboard.AppManagement.StopRouteDetails.StopRouteDetailsGetStops
  )

type StopRouteDetailsGetStop =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/STOP_ROUTE_DETAILS/STOP_ROUTE_DETAILS_GET_STOP"
      :> API.Types.Dashboard.AppManagement.StopRouteDetails.StopRouteDetailsGetStop
  )

type StopRouteDetailsGetRouteStopMappingByStop =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/STOP_ROUTE_DETAILS/STOP_ROUTE_DETAILS_GET_ROUTE_STOP_MAPPING_BY_STOP"
      :> API.Types.Dashboard.AppManagement.StopRouteDetails.StopRouteDetailsGetRouteStopMappingByStop
  )

type StopRouteDetailsGetRouteStopMappingByRoute =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/STOP_ROUTE_DETAILS/STOP_ROUTE_DETAILS_GET_ROUTE_STOP_MAPPING_BY_ROUTE"
      :> API.Types.Dashboard.AppManagement.StopRouteDetails.StopRouteDetailsGetRouteStopMappingByRoute
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = stopRouteDetailsGetStops merchantId city :<|> stopRouteDetailsGetStop merchantId city :<|> stopRouteDetailsGetRouteStopMappingByStop merchantId city :<|> stopRouteDetailsGetRouteStopMappingByRoute merchantId city

stopRouteDetailsGetStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Domain.Types.Station.Station])
stopRouteDetailsGetStops a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.StopRouteDetails.stopRouteDetailsGetStops a5 a4 a2 a1

stopRouteDetailsGetStop :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler API.Types.Dashboard.AppManagement.StopRouteDetails.StationResp)
stopRouteDetailsGetStop a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.StopRouteDetails.stopRouteDetailsGetStop a5 a4 a2 a1

stopRouteDetailsGetRouteStopMappingByStop :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Domain.Types.RouteStopMapping.RouteStopMapping])
stopRouteDetailsGetRouteStopMappingByStop a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.StopRouteDetails.stopRouteDetailsGetRouteStopMappingByStop a5 a4 a2 a1

stopRouteDetailsGetRouteStopMappingByRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler API.Types.Dashboard.AppManagement.StopRouteDetails.RouteStopMappingWithPolyline)
stopRouteDetailsGetRouteStopMappingByRoute a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.StopRouteDetails.stopRouteDetailsGetRouteStopMappingByRoute a5 a4 a2 a1
