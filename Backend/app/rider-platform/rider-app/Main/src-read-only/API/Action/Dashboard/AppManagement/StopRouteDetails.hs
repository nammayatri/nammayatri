{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.StopRouteDetails
  ( API.Types.Dashboard.AppManagement.StopRouteDetails.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.StopRouteDetails.API)
handler merchantId city = stopRouteDetailsGetStops merchantId city :<|> stopRouteDetailsGetStop merchantId city :<|> stopRouteDetailsGetRouteStopMappingByStop merchantId city :<|> stopRouteDetailsGetRouteStopMappingByRoute merchantId city

stopRouteDetailsGetStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Domain.Types.Station.Station])
stopRouteDetailsGetStops a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.StopRouteDetails.stopRouteDetailsGetStops a4 a3 a2 a1

stopRouteDetailsGetStop :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler API.Types.Dashboard.AppManagement.StopRouteDetails.StationResp)
stopRouteDetailsGetStop a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.StopRouteDetails.stopRouteDetailsGetStop a4 a3 a2 a1

stopRouteDetailsGetRouteStopMappingByStop :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Domain.Types.RouteStopMapping.RouteStopMapping])
stopRouteDetailsGetRouteStopMappingByStop a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.StopRouteDetails.stopRouteDetailsGetRouteStopMappingByStop a4 a3 a2 a1

stopRouteDetailsGetRouteStopMappingByRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler API.Types.Dashboard.AppManagement.StopRouteDetails.RouteStopMappingWithPolyline)
stopRouteDetailsGetRouteStopMappingByRoute a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.StopRouteDetails.stopRouteDetailsGetRouteStopMappingByRoute a4 a3 a2 a1
