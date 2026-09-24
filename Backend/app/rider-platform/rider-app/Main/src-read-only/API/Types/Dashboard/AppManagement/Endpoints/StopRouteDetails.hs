{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.StopRouteDetails where

import qualified "beckn-spec" BecknV2.OnDemand.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Types.RouteStopMapping
import qualified "this" Domain.Types.Station
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import Servant
import Servant.Client

data RouteStopMappingWithPolyline = RouteStopMappingWithPolyline {polyline :: Kernel.Prelude.Maybe Kernel.Prelude.Text, routeStopMappings :: [Domain.Types.RouteStopMapping.RouteStopMapping]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StationResp = StationResp {station :: Kernel.Prelude.Maybe Domain.Types.Station.Station}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("stopRouteDetails" :> (StopRouteDetailsGetStops :<|> StopRouteDetailsGetStop :<|> StopRouteDetailsGetRouteStopMappingByStop :<|> StopRouteDetailsGetRouteStopMappingByRoute))

type StopRouteDetailsGetStops =
  ( "stops" :> QueryParam "includeClusterId" Kernel.Prelude.Bool :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           '[JSON]
           [Domain.Types.Station.Station]
  )

type StopRouteDetailsGetStop = ("stop" :> Capture "stopCode" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get '[JSON] StationResp)

type StopRouteDetailsGetRouteStopMappingByStop =
  ( "route-stop-mapping" :> "stop" :> Capture "stopCode" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get '[JSON] [Domain.Types.RouteStopMapping.RouteStopMapping]
  )

type StopRouteDetailsGetRouteStopMappingByRoute =
  ( "route-stop-mapping" :> "route" :> Capture "routeCode" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get '[JSON] RouteStopMappingWithPolyline
  )

data StopRouteDetailsAPIs = StopRouteDetailsAPIs
  { stopRouteDetailsGetStops :: Kernel.Prelude.Maybe Kernel.Prelude.Bool -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Domain.Types.Station.Station],
    stopRouteDetailsGetStop :: Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient StationResp,
    stopRouteDetailsGetRouteStopMappingByStop :: Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Domain.Types.RouteStopMapping.RouteStopMapping],
    stopRouteDetailsGetRouteStopMappingByRoute :: Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient RouteStopMappingWithPolyline
  }

mkStopRouteDetailsAPIs :: (Client EulerHS.Types.EulerClient API -> StopRouteDetailsAPIs)
mkStopRouteDetailsAPIs stopRouteDetailsClient = (StopRouteDetailsAPIs {..})
  where
    stopRouteDetailsGetStops :<|> stopRouteDetailsGetStop :<|> stopRouteDetailsGetRouteStopMappingByStop :<|> stopRouteDetailsGetRouteStopMappingByRoute = stopRouteDetailsClient

data StopRouteDetailsUserActionType
  = STOP_ROUTE_DETAILS_GET_STOPS
  | STOP_ROUTE_DETAILS_GET_STOP
  | STOP_ROUTE_DETAILS_GET_ROUTE_STOP_MAPPING_BY_STOP
  | STOP_ROUTE_DETAILS_GET_ROUTE_STOP_MAPPING_BY_ROUTE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''StopRouteDetailsUserActionType])
