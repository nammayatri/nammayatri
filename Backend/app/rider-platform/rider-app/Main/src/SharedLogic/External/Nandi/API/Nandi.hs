module SharedLogic.External.Nandi.API.Nandi where

import Data.Aeson
import qualified EulerHS.Types as ET
import Kernel.Prelude
import Servant
import SharedLogic.External.Nandi.Types

type RouteStopMappingByRouteIdAPI = "route-stop-mapping" :> Capture "gtfs_id" Text :> "route" :> Capture "route_code" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

type RouteStopMappingByStopCodeAPI = "route-stop-mapping" :> Capture "gtfs_id" Text :> "stop" :> Capture "stop_code" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

type RouteByRouteIdAPI = "route" :> Capture "gtfs_id" Text :> Capture "route_id" Text :> Get '[JSON] RouteInfoNandi

type RouteFuzzySearchAPI = "routes" :> Capture "gtfs_id" Text :> "fuzzy" :> Capture "query" Text :> Get '[JSON] [RouteInfoNandi]

type RoutesByGtfsIdAPI = "routes" :> Capture "gtfs_id" Text :> Get '[JSON] [RouteInfoNandi]

type StopsByGtfsIdAPI = "stops" :> Capture "gtfs_id" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

type StopsByGtfsIdAndStopCodeAPI = "stop" :> Capture "gtfs_id" Text :> Capture "stop_code" Text :> Get '[JSON] RouteStopMappingInMemoryServer

type StopsByGtfsIdFuzzySearchAPI = "stops" :> Capture "gtfs_id" Text :> "fuzzy" :> Capture "query" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

type VehicleServiceTypeAPI = "vehicle" :> Capture "vehicle_number" Text :> "service-type" :> Get '[JSON] VehicleServiceTypeResponse

type StopChildrenAPI = "station-children" :> Capture "gtfs_id" Text :> Capture "stop_code" Text :> Get '[JSON] [Text]

type GtfsVersionAPI = "version" :> Capture "gtfs_id" Text :> Get '[JSON] Text

type GtfsGraphQLAPI = "graphql" :> ReqBody '[JSON] GtfsGraphQLRequest :> Post '[JSON] Value

type StopCodeAPI = "stop-code" :> Capture "gtfs_id" Text :> Capture "provider_stop_code" Text :> Get '[JSON] StopCodeResponse

type TripInfoAPI = "trip" :> Capture "trip_id" Text :> Get '[JSON] TripInfoResponse

type RouteStopMappingByStopCodesAPI = "getAllRouteStopMappingsByStopCodes" :> ReqBody '[JSON] RouteStopMappingByStopCodesReq :> Post '[JSON] [RouteStopMappingInMemoryServer]

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

nandiStopsByGtfsIdAPI :: Proxy StopsByGtfsIdAPI
nandiStopsByGtfsIdAPI = Proxy

nandiStopsByGtfsIdAndStopCodeAPI :: Proxy StopsByGtfsIdAndStopCodeAPI
nandiStopsByGtfsIdAndStopCodeAPI = Proxy

nandiStopsByGtfsIdFuzzySearchAPI :: Proxy StopsByGtfsIdFuzzySearchAPI
nandiStopsByGtfsIdFuzzySearchAPI = Proxy

nandiVehicleServiceTypeAPI :: Proxy VehicleServiceTypeAPI
nandiVehicleServiceTypeAPI = Proxy

nandiStopChildrenAPI :: Proxy StopChildrenAPI
nandiStopChildrenAPI = Proxy

nandiGtfsVersionAPI :: Proxy GtfsVersionAPI
nandiGtfsVersionAPI = Proxy

nandiGtfsGraphQLAPI :: Proxy GtfsGraphQLAPI
nandiGtfsGraphQLAPI = Proxy

nandiStopCodeAPI :: Proxy StopCodeAPI
nandiStopCodeAPI = Proxy

nandiTripInfoAPI :: Proxy TripInfoAPI
nandiTripInfoAPI = Proxy

nandiRouteStopMappingByStopCodesAPI :: Proxy RouteStopMappingByStopCodesAPI
nandiRouteStopMappingByStopCodesAPI = Proxy

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

getNandiStopsByGtfsId :: Text -> ET.EulerClient [RouteStopMappingInMemoryServer]
getNandiStopsByGtfsId = ET.client nandiStopsByGtfsIdAPI

getNandiStopsByGtfsIdAndStopCode :: Text -> Text -> ET.EulerClient RouteStopMappingInMemoryServer
getNandiStopsByGtfsIdAndStopCode = ET.client nandiStopsByGtfsIdAndStopCodeAPI

getNandiStopsByGtfsIdFuzzySearch :: Text -> Text -> ET.EulerClient [RouteStopMappingInMemoryServer]
getNandiStopsByGtfsIdFuzzySearch = ET.client nandiStopsByGtfsIdFuzzySearchAPI

getNandiVehicleServiceType :: Text -> ET.EulerClient VehicleServiceTypeResponse
getNandiVehicleServiceType = ET.client nandiVehicleServiceTypeAPI

getNandiStopChildren :: Text -> Text -> ET.EulerClient [Text]
getNandiStopChildren = ET.client nandiStopChildrenAPI

getNandiGtfsVersion :: Text -> ET.EulerClient Text
getNandiGtfsVersion = ET.client nandiGtfsVersionAPI

postNandiGtfsGraphQL :: GtfsGraphQLRequest -> ET.EulerClient Value
postNandiGtfsGraphQL = ET.client nandiGtfsGraphQLAPI

getNandiStopCode :: Text -> Text -> ET.EulerClient StopCodeResponse
getNandiStopCode = ET.client nandiStopCodeAPI

getNandiTripInfo :: Text -> ET.EulerClient TripInfoResponse
getNandiTripInfo = ET.client nandiTripInfoAPI

postNandiRouteStopMappingByStopCodes :: RouteStopMappingByStopCodesReq -> ET.EulerClient [RouteStopMappingInMemoryServer]
postNandiRouteStopMappingByStopCodes = ET.client nandiRouteStopMappingByStopCodesAPI
