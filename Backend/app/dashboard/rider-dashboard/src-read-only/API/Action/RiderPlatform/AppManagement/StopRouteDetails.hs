{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.StopRouteDetails
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.StopRouteDetails
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Domain.Action.RiderPlatform.AppManagement.StopRouteDetails
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.RouteStopMapping
import qualified "rider-app" Domain.Types.Station
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("stopRouteDetails" :> (StopRouteDetailsGetStops :<|> StopRouteDetailsGetStop :<|> StopRouteDetailsGetRouteStopMappingByStop :<|> StopRouteDetailsGetRouteStopMappingByRoute))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = stopRouteDetailsGetStops merchantId city :<|> stopRouteDetailsGetStop merchantId city :<|> stopRouteDetailsGetRouteStopMappingByStop merchantId city :<|> stopRouteDetailsGetRouteStopMappingByRoute merchantId city

type StopRouteDetailsGetStops =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.STOP_ROUTE_DETAILS / 'API.Types.Dashboard.AppManagement.StopRouteDetails.STOP_ROUTE_DETAILS_GET_STOPS)
      :> API.Types.Dashboard.AppManagement.StopRouteDetails.StopRouteDetailsGetStops
  )

type StopRouteDetailsGetStop =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.STOP_ROUTE_DETAILS / 'API.Types.Dashboard.AppManagement.StopRouteDetails.STOP_ROUTE_DETAILS_GET_STOP)
      :> API.Types.Dashboard.AppManagement.StopRouteDetails.StopRouteDetailsGetStop
  )

type StopRouteDetailsGetRouteStopMappingByStop =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.STOP_ROUTE_DETAILS / 'API.Types.Dashboard.AppManagement.StopRouteDetails.STOP_ROUTE_DETAILS_GET_ROUTE_STOP_MAPPING_BY_STOP)
      :> API.Types.Dashboard.AppManagement.StopRouteDetails.StopRouteDetailsGetRouteStopMappingByStop
  )

type StopRouteDetailsGetRouteStopMappingByRoute =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.STOP_ROUTE_DETAILS / 'API.Types.Dashboard.AppManagement.StopRouteDetails.STOP_ROUTE_DETAILS_GET_ROUTE_STOP_MAPPING_BY_ROUTE)
      :> API.Types.Dashboard.AppManagement.StopRouteDetails.StopRouteDetailsGetRouteStopMappingByRoute
  )

stopRouteDetailsGetStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Domain.Types.Station.Station])
stopRouteDetailsGetStops merchantShortId opCity apiTokenInfo includeClusterId vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.StopRouteDetails.stopRouteDetailsGetStops merchantShortId opCity apiTokenInfo includeClusterId vehicleCategory

stopRouteDetailsGetStop :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler API.Types.Dashboard.AppManagement.StopRouteDetails.StationResp)
stopRouteDetailsGetStop merchantShortId opCity apiTokenInfo stopCode vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.StopRouteDetails.stopRouteDetailsGetStop merchantShortId opCity apiTokenInfo stopCode vehicleCategory

stopRouteDetailsGetRouteStopMappingByStop :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Domain.Types.RouteStopMapping.RouteStopMapping])
stopRouteDetailsGetRouteStopMappingByStop merchantShortId opCity apiTokenInfo stopCode vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.StopRouteDetails.stopRouteDetailsGetRouteStopMappingByStop merchantShortId opCity apiTokenInfo stopCode vehicleCategory

stopRouteDetailsGetRouteStopMappingByRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler API.Types.Dashboard.AppManagement.StopRouteDetails.RouteStopMappingWithPolyline)
stopRouteDetailsGetRouteStopMappingByRoute merchantShortId opCity apiTokenInfo routeCode vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.StopRouteDetails.stopRouteDetailsGetRouteStopMappingByRoute merchantShortId opCity apiTokenInfo routeCode vehicleCategory
