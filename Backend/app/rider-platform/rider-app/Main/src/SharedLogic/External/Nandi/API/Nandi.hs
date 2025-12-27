module SharedLogic.External.Nandi.API.Nandi where

import Data.Aeson
import qualified EulerHS.Types as ET
import Kernel.Prelude
import Servant
import SharedLogic.External.Nandi.Types

type RouteStopMappingByRouteIdAPI = "route-stop-mapping" :> Capture "gtfs_id" Text :> "route" :> Capture "route_code" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

type RouteStopMappingByStopCodeAPI = "route-stop-mapping" :> Capture "gtfs_id" Text :> "stop" :> Capture "stop_code" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

type RouteByRouteIdAPI = "route" :> Capture "gtfs_id" Text :> Capture "route_id" Text :> Get '[JSON] RouteInfoNandi

type RoutesByRouteIdsAPI = "getRoutesByIds" :> Capture "gtfs_id" Text :> ReqBody '[JSON] [Text] :> Post '[JSON] [RouteInfoNandi]

type BusRouteScheduleAPI = "bus-route-schedule" :> Capture "gtfs_id" Text :> Capture "route_id" Text :> Get '[JSON] BusScheduleDetails

type RouteFuzzySearchAPI = "routes" :> Capture "gtfs_id" Text :> "fuzzy" :> Capture "query" Text :> Get '[JSON] [RouteInfoNandi]

type RoutesByGtfsIdAPI = "routes" :> Capture "gtfs_id" Text :> Get '[JSON] [RouteInfoNandi]

type StopsByGtfsIdAPI = "stops" :> Capture "gtfs_id" Text :> Get '[JSON] [RouteStopMappingInMemoryServerWithPublicData]

type StopsByGtfsIdAndStopCodeAPI = "stop" :> Capture "gtfs_id" Text :> Capture "stop_code" Text :> Get '[JSON] RouteStopMappingInMemoryServer

type StopsByGtfsIdFuzzySearchAPI = "stops" :> Capture "gtfs_id" Text :> "fuzzy" :> Capture "query" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

type VehicleServiceTypeAPI = "vehicle" :> Capture "gtfs_id" Text :> "service-type" :> Capture "vehicle_number" Text :> QueryParam "passVerifyReq" Bool :> Get '[JSON] VehicleServiceTypeResponse

type VehiclesByServiceTypeAPI = "vehicles" :> Capture "gtfs_id" Text :> "list" :> "service-tier" :> Capture "service_type" Text :> Get '[JSON] [Text]

type StopChildrenAPI = "station-children" :> Capture "gtfs_id" Text :> Capture "stop_code" Text :> Get '[JSON] [Text]

type GtfsVersionAPI = "version" :> Capture "gtfs_id" Text :> Get '[JSON] Text

type GtfsGraphQLAPI = "graphql" :> ReqBody '[JSON] GtfsGraphQLRequest :> Post '[JSON] Value

type StopCodeAPI = "stop-code" :> Capture "gtfs_id" Text :> Capture "provider_stop_code" Text :> Get '[JSON] StopCodeResponse

type TripInfoAPI = "trip" :> Capture "trip_id" Text :> Get '[JSON] TripInfoResponse

type RouteStopMappingByStopCodesAPI = "getAllRouteStopMappingsByStopCodes" :> ReqBody '[JSON] RouteStopMappingByStopCodesReq :> Post '[JSON] [RouteStopMappingInMemoryServer]

type ExampleTripAPI = "example-trip" :> Capture "gtfs_id" Text :> Capture "route_id" Text :> Get '[JSON] TripDetails

type VehicleInfoAPI = "vehicle" :> Capture "gtfs_id" Text :> Capture "vehicle_no" Text :> "info" :> Get '[JSON] VehicleInfoResponse

type VehicleOperationDataAPI = "vehicle-operation-data" :> Capture "fleet_no" Text :> Get '[JSON] VehicleOperationInfo

type DepotNamesAPI = "depotNames" :> Get '[JSON] [Text]

type DepotIdsAPI = "depotIds" :> Get '[JSON] [Text]

type GetVehiclesFromDepotNameAPI = "getVehiclesFrom" :> QueryParam "depotName" Text :> Get '[JSON] [DepotVehicle]

type GetVehiclesFromDepotIdAPI = "getVehiclesFrom" :> QueryParam "depotId" Text :> Get '[JSON] [DepotVehicle]

type DepotNameByIdAPI = "getDepotNameById" :> Capture "depotId" Text :> Get '[JSON] Text

nandiGetRouteStopMappingByRouteIdAPI :: Proxy RouteStopMappingByRouteIdAPI
nandiGetRouteStopMappingByRouteIdAPI = Proxy

nandiGetRouteStopMappingByStopCodeAPI :: Proxy RouteStopMappingByStopCodeAPI
nandiGetRouteStopMappingByStopCodeAPI = Proxy

nandiRouteByRouteIdAPI :: Proxy RouteByRouteIdAPI
nandiRouteByRouteIdAPI = Proxy

nandiRoutesByRouteIdsAPI :: Proxy RoutesByRouteIdsAPI
nandiRoutesByRouteIdsAPI = Proxy

nandiBusRouteScheduleAPI :: Proxy BusRouteScheduleAPI
nandiBusRouteScheduleAPI = Proxy

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

nandiVehiclesByServiceTypeAPI :: Proxy VehiclesByServiceTypeAPI
nandiVehiclesByServiceTypeAPI = Proxy

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

nandiExampleTripAPI :: Proxy ExampleTripAPI
nandiExampleTripAPI = Proxy

nandiVehicleInfoAPI :: Proxy VehicleInfoAPI
nandiVehicleInfoAPI = Proxy

nandiVehicleOperationDataAPI :: Proxy VehicleOperationDataAPI
nandiVehicleOperationDataAPI = Proxy

nandiDepotNamesAPI :: Proxy DepotNamesAPI
nandiDepotNamesAPI = Proxy

nandiDepotIdsAPI :: Proxy DepotIdsAPI
nandiDepotIdsAPI = Proxy

nandiGetVehiclesFromDepotNameAPI :: Proxy GetVehiclesFromDepotNameAPI
nandiGetVehiclesFromDepotNameAPI = Proxy

nandiGetVehiclesFromDepotIdAPI :: Proxy GetVehiclesFromDepotIdAPI
nandiGetVehiclesFromDepotIdAPI = Proxy

nandiDepotNameByIdAPI :: Proxy DepotNameByIdAPI
nandiDepotNameByIdAPI = Proxy

getNandiGetRouteStopMappingByRouteId :: Text -> Text -> ET.EulerClient [RouteStopMappingInMemoryServer]
getNandiGetRouteStopMappingByRouteId = ET.client nandiGetRouteStopMappingByRouteIdAPI

getNandiGetRouteStopMappingByStopCode :: Text -> Text -> ET.EulerClient [RouteStopMappingInMemoryServer]
getNandiGetRouteStopMappingByStopCode = ET.client nandiGetRouteStopMappingByStopCodeAPI

getNandiRouteByRouteId :: Text -> Text -> ET.EulerClient RouteInfoNandi
getNandiRouteByRouteId = ET.client nandiRouteByRouteIdAPI

getNandiRoutesByRouteIds :: Text -> [Text] -> ET.EulerClient [RouteInfoNandi]
getNandiRoutesByRouteIds = ET.client nandiRoutesByRouteIdsAPI

getNandiBusRouteSchedule :: Text -> Text -> ET.EulerClient BusScheduleDetails
getNandiBusRouteSchedule = ET.client nandiBusRouteScheduleAPI

getNandiRouteFuzzySearch :: Text -> Text -> ET.EulerClient [RouteInfoNandi]
getNandiRouteFuzzySearch = ET.client nandiRouteFuzzySearchAPI

getNandiRoutesByGtfsId :: Text -> ET.EulerClient [RouteInfoNandi]
getNandiRoutesByGtfsId = ET.client nandiRoutesByGtfsIdAPI

getNandiStopsByGtfsId :: Text -> ET.EulerClient [RouteStopMappingInMemoryServerWithPublicData]
getNandiStopsByGtfsId = ET.client nandiStopsByGtfsIdAPI

getNandiStopsByGtfsIdAndStopCode :: Text -> Text -> ET.EulerClient RouteStopMappingInMemoryServer
getNandiStopsByGtfsIdAndStopCode = ET.client nandiStopsByGtfsIdAndStopCodeAPI

getNandiStopsByGtfsIdFuzzySearch :: Text -> Text -> ET.EulerClient [RouteStopMappingInMemoryServer]
getNandiStopsByGtfsIdFuzzySearch = ET.client nandiStopsByGtfsIdFuzzySearchAPI

getNandiVehicleServiceType :: Text -> Text -> Maybe Bool -> ET.EulerClient VehicleServiceTypeResponse
getNandiVehicleServiceType = ET.client nandiVehicleServiceTypeAPI

getNandiVehiclesByServiceType :: Text -> Text -> ET.EulerClient [Text]
getNandiVehiclesByServiceType = ET.client nandiVehiclesByServiceTypeAPI

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

getNandiExampleTrip :: Text -> Text -> ET.EulerClient TripDetails
getNandiExampleTrip = ET.client nandiExampleTripAPI

getNandiVehicleInfo :: Text -> Text -> ET.EulerClient VehicleInfoResponse
getNandiVehicleInfo = ET.client nandiVehicleInfoAPI

getNandiVehicleOperationData :: Text -> ET.EulerClient VehicleOperationInfo
getNandiVehicleOperationData = ET.client nandiVehicleOperationDataAPI

getNandiDepotNames :: ET.EulerClient [Text]
getNandiDepotNames = ET.client nandiDepotNamesAPI

getNandiDepotIds :: ET.EulerClient [Text]
getNandiDepotIds = ET.client nandiDepotIdsAPI

getNandiGetVehiclesFromByDepotName :: Maybe Text -> ET.EulerClient [DepotVehicle]
getNandiGetVehiclesFromByDepotName = ET.client nandiGetVehiclesFromDepotNameAPI

getNandiGetVehiclesFromByDepotId :: Maybe Text -> ET.EulerClient [DepotVehicle]
getNandiGetVehiclesFromByDepotId = ET.client nandiGetVehiclesFromDepotIdAPI

getNandiDepotNameById :: Text -> ET.EulerClient Text
getNandiDepotNameById = ET.client nandiDepotNameByIdAPI
