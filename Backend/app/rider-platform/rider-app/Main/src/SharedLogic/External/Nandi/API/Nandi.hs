module SharedLogic.External.Nandi.API.Nandi where

import qualified EulerHS.Types as ET
import Kernel.Prelude
import Servant
import SharedLogic.External.Nandi.Types

type RouteStopMappingByRouteIdAPI = "route-stop-mapping" :> Capture "gtfs_id" Text :> "route" :> Capture "route_code" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

type RouteStopMappingByStopCodeAPI = "route-stop-mapping" :> Capture "gtfs_id" Text :> "stop" :> Capture "stop_code" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

type RouteByRouteIdAPI = "route" :> Capture "gtfs_id" Text :> Capture "route_id" Text :> Get '[JSON] RouteInfoNandi

type RouteFuzzySearchAPI = "routes" :> Capture "gtfs_id" Text :> "fuzzy" :> Capture "query" Text :> Get '[JSON] [RouteInfoNandi]

type RoutesByGtfsIdAPI = "routes" :> Capture "gtfs_id" Text :> Get '[JSON] [RouteInfoNandi]

nandiGetRouteStopMappingByRouteIdAPI :: Proxy RouteStopMappingByRouteIdAPI
nandiGetRouteStopMappingByRouteIdAPI = Proxy

nandiGetRouteStopMappingByStopCodeAPI :: Proxy RouteStopMappingByStopCodeAPI
nandiGetRouteStopMappingByStopCodeAPI = Proxy

nandiRouteByRouteIdAPI :: Proxy RouteByRouteIdAPI
nandiRouteByRouteIdAPI = Proxy

nandiRouteFuzzySearchAPI :: Proxy RouteFuzzySearchAPI
nandiRouteFuzzySearchAPI = Proxy

nandiRoutesByGtfsIdAPI :: Proxy RoutesByGtfsIdAPI
nandiRoutesByGtfsIdAPI = Proxy

getNandiGetRouteStopMappingByRouteId :: Text -> Text -> ET.EulerClient [RouteStopMappingInMemoryServer]
getNandiGetRouteStopMappingByRouteId = ET.client nandiGetRouteStopMappingByRouteIdAPI

getNandiGetRouteStopMappingByStopCode :: Text -> Text -> ET.EulerClient [RouteStopMappingInMemoryServer]
getNandiGetRouteStopMappingByStopCode = ET.client nandiGetRouteStopMappingByStopCodeAPI

getNandiRouteByRouteId :: Text -> Text -> ET.EulerClient RouteInfoNandi
getNandiRouteByRouteId = ET.client nandiRouteByRouteIdAPI

getNandiRouteFuzzySearch :: Text -> Text -> ET.EulerClient [RouteInfoNandi]
getNandiRouteFuzzySearch = ET.client nandiRouteFuzzySearchAPI

getNandiRoutesByGtfsId :: Text -> ET.EulerClient [RouteInfoNandi]
getNandiRoutesByGtfsId = ET.client nandiRoutesByGtfsIdAPI
