{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.AppManagement.StopRouteDetails
  ( stopRouteDetailsGetStops,
    stopRouteDetailsGetStop,
    stopRouteDetailsGetRouteStopMappingByStop,
    stopRouteDetailsGetRouteStopMappingByRoute,
  )
where

import qualified API.Client.RiderPlatform.AppManagement
import qualified API.Types.Dashboard.AppManagement.StopRouteDetails
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.RouteStopMapping
import qualified "rider-app" Domain.Types.Station
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

stopRouteDetailsGetStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Domain.Types.Station.Station])
stopRouteDetailsGetStops merchantShortId opCity apiTokenInfo includeClusterId vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.stopRouteDetailsDSL.stopRouteDetailsGetStops) includeClusterId vehicleCategory

stopRouteDetailsGetStop :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow API.Types.Dashboard.AppManagement.StopRouteDetails.StationResp)
stopRouteDetailsGetStop merchantShortId opCity apiTokenInfo stopCode vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.stopRouteDetailsDSL.stopRouteDetailsGetStop) stopCode vehicleCategory

stopRouteDetailsGetRouteStopMappingByStop :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Domain.Types.RouteStopMapping.RouteStopMapping])
stopRouteDetailsGetRouteStopMappingByStop merchantShortId opCity apiTokenInfo stopCode vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.stopRouteDetailsDSL.stopRouteDetailsGetRouteStopMappingByStop) stopCode vehicleCategory

stopRouteDetailsGetRouteStopMappingByRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow API.Types.Dashboard.AppManagement.StopRouteDetails.RouteStopMappingWithPolyline)
stopRouteDetailsGetRouteStopMappingByRoute merchantShortId opCity apiTokenInfo routeCode vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.stopRouteDetailsDSL.stopRouteDetailsGetRouteStopMappingByRoute) routeCode vehicleCategory
