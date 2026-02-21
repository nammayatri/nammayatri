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

type AlternateStopsByGtfsIdAndStopCodeAPI = "alternateStops" :> Capture "gtfs_id" Text :> Capture "stop_code" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

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

nandiAlternateStopsByGtfsIdAndStopCodeAPI :: Proxy AlternateStopsByGtfsIdAndStopCodeAPI
nandiAlternateStopsByGtfsIdAndStopCodeAPI = Proxy

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

getNandiAlternateStopsByGtfsIdAndStopCode :: Text -> Text -> ET.EulerClient [RouteStopMappingInMemoryServer]
getNandiAlternateStopsByGtfsIdAndStopCode = ET.client nandiAlternateStopsByGtfsIdAndStopCodeAPI

type OperatorGetRowAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "crud" :> Capture "table" Text :> QueryParam "column" Text :> Get '[JSON] Value

type OperatorGetAllRowsAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "crud" :> Capture "table" Text :> "all" :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [Value]

type OperatorDeleteRowAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "crud" :> Capture "table" Text :> "delete" :> ReqBody '[JSON] Value :> Post '[JSON] RowsAffectedResp

type OperatorUpsertRowAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "crud" :> Capture "table" Text :> "upsert" :> ReqBody '[JSON] Value :> Post '[JSON] Value

type OperatorServiceTypesAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "service-types" :> Get '[JSON] [ServiceType]

type OperatorRoutesAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "routes" :> Get '[JSON] [NandiRoute]

type OperatorDepotsAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "depots" :> Get '[JSON] [Depot]

type OperatorShiftTypesAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "shift-types" :> Get '[JSON] [Text]

type OperatorScheduleNumbersAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "schedule-numbers" :> Get '[JSON] [ScheduleNumber]

type OperatorDayTypesAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "day-types" :> Get '[JSON] [Text]

type OperatorTripTypesAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "trip-types" :> Get '[JSON] [Text]

type OperatorBreakTypesAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "break-types" :> Get '[JSON] [Text]

type OperatorTripDetailsAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "trip-details" :> QueryParam "scheduleNumber" Text :> Get '[JSON] [NandiTripDetail]

type OperatorFleetsAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "fleets" :> Get '[JSON] [Fleet]

type OperatorConductorsAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "conductors" :> QueryParam "token" Text :> Get '[JSON] Employee

type OperatorDriversAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "drivers" :> QueryParam "token" Text :> Get '[JSON] Employee

type OperatorDeviceIdsAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "device-ids" :> Get '[JSON] [Text]

type OperatorTabletIdsAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "tablet-ids" :> Get '[JSON] [Text]

type OperatorOperatorsAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "operators" :> QueryParam "role" Text :> Get '[JSON] [Employee]

type OperatorWaybillStatusAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "waybill" :> "status" :> ReqBody '[JSON] UpdateWaybillStatusReq :> Post '[JSON] RowsAffectedResp

type OperatorWaybillFleetAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "waybill" :> "fleet" :> ReqBody '[JSON] UpdateWaybillFleetReq :> Post '[JSON] RowsAffectedResp

type OperatorWaybillTabletAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "waybill" :> "tablet" :> ReqBody '[JSON] UpdateWaybillTabletReq :> Post '[JSON] RowsAffectedResp

type OperatorWaybillsAPI = "internal" :> "operator" :> Capture "gtfs_id" Text :> "waybills" :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [Value]

operatorGetRowAPI :: Proxy OperatorGetRowAPI
operatorGetRowAPI = Proxy

operatorGetAllRowsAPI :: Proxy OperatorGetAllRowsAPI
operatorGetAllRowsAPI = Proxy

operatorDeleteRowAPI :: Proxy OperatorDeleteRowAPI
operatorDeleteRowAPI = Proxy

operatorUpsertRowAPI :: Proxy OperatorUpsertRowAPI
operatorUpsertRowAPI = Proxy

operatorServiceTypesAPI :: Proxy OperatorServiceTypesAPI
operatorServiceTypesAPI = Proxy

operatorRoutesAPI :: Proxy OperatorRoutesAPI
operatorRoutesAPI = Proxy

operatorDepotsAPI :: Proxy OperatorDepotsAPI
operatorDepotsAPI = Proxy

operatorShiftTypesAPI :: Proxy OperatorShiftTypesAPI
operatorShiftTypesAPI = Proxy

operatorScheduleNumbersAPI :: Proxy OperatorScheduleNumbersAPI
operatorScheduleNumbersAPI = Proxy

operatorDayTypesAPI :: Proxy OperatorDayTypesAPI
operatorDayTypesAPI = Proxy

operatorTripTypesAPI :: Proxy OperatorTripTypesAPI
operatorTripTypesAPI = Proxy

operatorBreakTypesAPI :: Proxy OperatorBreakTypesAPI
operatorBreakTypesAPI = Proxy

operatorTripDetailsAPI :: Proxy OperatorTripDetailsAPI
operatorTripDetailsAPI = Proxy

operatorFleetsAPI :: Proxy OperatorFleetsAPI
operatorFleetsAPI = Proxy

operatorConductorsAPI :: Proxy OperatorConductorsAPI
operatorConductorsAPI = Proxy

operatorDriversAPI :: Proxy OperatorDriversAPI
operatorDriversAPI = Proxy

operatorDeviceIdsAPI :: Proxy OperatorDeviceIdsAPI
operatorDeviceIdsAPI = Proxy

operatorTabletIdsAPI :: Proxy OperatorTabletIdsAPI
operatorTabletIdsAPI = Proxy

operatorOperatorsAPI :: Proxy OperatorOperatorsAPI
operatorOperatorsAPI = Proxy

operatorWaybillStatusAPI :: Proxy OperatorWaybillStatusAPI
operatorWaybillStatusAPI = Proxy

operatorWaybillFleetAPI :: Proxy OperatorWaybillFleetAPI
operatorWaybillFleetAPI = Proxy

operatorWaybillTabletAPI :: Proxy OperatorWaybillTabletAPI
operatorWaybillTabletAPI = Proxy

operatorWaybillsAPI :: Proxy OperatorWaybillsAPI
operatorWaybillsAPI = Proxy

-- Client functions
getOperatorRow :: Text -> Text -> Maybe Text -> ET.EulerClient Value
getOperatorRow = ET.client operatorGetRowAPI

getOperatorAllRows :: Text -> Text -> Maybe Int -> Maybe Int -> ET.EulerClient [Value]
getOperatorAllRows = ET.client operatorGetAllRowsAPI

postOperatorDeleteRow :: Text -> Text -> Value -> ET.EulerClient RowsAffectedResp
postOperatorDeleteRow = ET.client operatorDeleteRowAPI

postOperatorUpsertRow :: Text -> Text -> Value -> ET.EulerClient Value
postOperatorUpsertRow = ET.client operatorUpsertRowAPI

getOperatorServiceTypes :: Text -> ET.EulerClient [ServiceType]
getOperatorServiceTypes = ET.client operatorServiceTypesAPI

getOperatorRoutes :: Text -> ET.EulerClient [NandiRoute]
getOperatorRoutes = ET.client operatorRoutesAPI

getOperatorDepots :: Text -> ET.EulerClient [Depot]
getOperatorDepots = ET.client operatorDepotsAPI

getOperatorShiftTypes :: Text -> ET.EulerClient [Text]
getOperatorShiftTypes = ET.client operatorShiftTypesAPI

getOperatorScheduleNumbers :: Text -> ET.EulerClient [ScheduleNumber]
getOperatorScheduleNumbers = ET.client operatorScheduleNumbersAPI

getOperatorDayTypes :: Text -> ET.EulerClient [Text]
getOperatorDayTypes = ET.client operatorDayTypesAPI

getOperatorTripTypes :: Text -> ET.EulerClient [Text]
getOperatorTripTypes = ET.client operatorTripTypesAPI

getOperatorBreakTypes :: Text -> ET.EulerClient [Text]
getOperatorBreakTypes = ET.client operatorBreakTypesAPI

getOperatorTripDetails :: Text -> Maybe Text -> ET.EulerClient [NandiTripDetail]
getOperatorTripDetails = ET.client operatorTripDetailsAPI

getOperatorFleets :: Text -> ET.EulerClient [Fleet]
getOperatorFleets = ET.client operatorFleetsAPI

getOperatorConductors :: Text -> Maybe Text -> ET.EulerClient Employee
getOperatorConductors = ET.client operatorConductorsAPI

getOperatorDrivers :: Text -> Maybe Text -> ET.EulerClient Employee
getOperatorDrivers = ET.client operatorDriversAPI

getOperatorDeviceIds :: Text -> ET.EulerClient [Text]
getOperatorDeviceIds = ET.client operatorDeviceIdsAPI

getOperatorTabletIds :: Text -> ET.EulerClient [Text]
getOperatorTabletIds = ET.client operatorTabletIdsAPI

getOperatorOperators :: Text -> Maybe Text -> ET.EulerClient [Employee]
getOperatorOperators = ET.client operatorOperatorsAPI

postOperatorWaybillStatus :: Text -> UpdateWaybillStatusReq -> ET.EulerClient RowsAffectedResp
postOperatorWaybillStatus = ET.client operatorWaybillStatusAPI

postOperatorWaybillFleet :: Text -> UpdateWaybillFleetReq -> ET.EulerClient RowsAffectedResp
postOperatorWaybillFleet = ET.client operatorWaybillFleetAPI

postOperatorWaybillTablet :: Text -> UpdateWaybillTabletReq -> ET.EulerClient RowsAffectedResp
postOperatorWaybillTablet = ET.client operatorWaybillTabletAPI

getOperatorWaybills :: Text -> Maybe Int -> Maybe Int -> ET.EulerClient [Value]
getOperatorWaybills = ET.client operatorWaybillsAPI

