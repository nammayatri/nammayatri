module SharedLogic.External.Nandi.API.Nandi where

import qualified EulerHS.Types as ET
import Kernel.Prelude
import Servant
import SharedLogic.External.Nandi.Types

type NandiPatternsAPI = "patterns" :> Capture "gtfs_id" Text :> Get '[JSON] [NandiPattern]

type NandiGetSpecificPatternAPI = "pattern" :> Capture "patternId" Text :> Get '[JSON] NandiPatternDetails

type RoutesAPI = "routes" :> Capture "gtfs_id" Text :> Get '[JSON] [NandiRoutesRes]

type RouteStopMappingByRouteIdAPI = "route-stop-mapping" :> Capture "gtfs_id" Text :> "route" :> Capture "route_code" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

type RouteStopMappingByStopCodeAPI = "route-stop-mapping" :> Capture "gtfs_id" Text :> "stop" :> Capture "stop_code" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

nandiPatternsAPI :: Proxy NandiPatternsAPI
nandiPatternsAPI = Proxy

nandiGetSpecificPatternAPI :: Proxy NandiGetSpecificPatternAPI
nandiGetSpecificPatternAPI = Proxy

nandiGetRouteStopMappingByRouteIdAPI :: Proxy RouteStopMappingByRouteIdAPI
nandiGetRouteStopMappingByRouteIdAPI = Proxy

nandiGetRouteStopMappingByStopCodeAPI :: Proxy RouteStopMappingByStopCodeAPI
nandiGetRouteStopMappingByStopCodeAPI = Proxy

nandiRoutesAPI :: Proxy RoutesAPI
nandiRoutesAPI = Proxy

getNandiPatterns :: Text -> ET.EulerClient [NandiPattern]
getNandiPatterns = ET.client nandiPatternsAPI

getNandiGetSpecificPattern :: Text -> ET.EulerClient NandiPatternDetails
getNandiGetSpecificPattern = ET.client nandiGetSpecificPatternAPI

getNandiRoutes :: Text -> ET.EulerClient [NandiRoutesRes]
getNandiRoutes = ET.client nandiRoutesAPI

getNandiGetRouteStopMappingByRouteId :: Text -> Text -> ET.EulerClient [RouteStopMappingInMemoryServer]
getNandiGetRouteStopMappingByRouteId = ET.client nandiGetRouteStopMappingByRouteIdAPI

getNandiGetRouteStopMappingByStopCode :: Text -> Text -> ET.EulerClient [RouteStopMappingInMemoryServer]
getNandiGetRouteStopMappingByStopCode = ET.client nandiGetRouteStopMappingByStopCodeAPI
