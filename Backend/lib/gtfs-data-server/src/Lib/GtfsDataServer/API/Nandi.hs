module Lib.GtfsDataServer.API.Nandi where

import Data.Aeson
import Data.Time (Day)
import qualified EulerHS.Types as ET
import Kernel.Prelude
import Lib.GtfsDataServer.Types
import Servant

type RouteStopMappingByRouteIdAPI = "route-stop-mapping" :> Capture "gtfs_id" Text :> "route" :> Capture "route_code" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

type RouteStopMappingByStopCodeAPI = "route-stop-mapping" :> Capture "gtfs_id" Text :> "stop" :> Capture "stop_code" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

type RouteByRouteIdAPI = "route" :> Capture "gtfs_id" Text :> Capture "route_id" Text :> Get '[JSON] RouteInfoNandi

type ExampleTripAPI = "example-trip" :> Capture "gtfs_id" Text :> Capture "route_id" Text :> Get '[JSON] TripDetails

type BusTripScheduleAPI = "bus-trip-schedule" :> Capture "gtfs_id" Text :> Capture "waybill_no" Text :> Capture "trip_number" Int :> Capture "route_id" Text :> Get '[JSON] BusScheduleDetails

type OperatorCurrentOperationAPI =
  "internal" :> "fleet-operator" :> Capture "gtfs_id" Text :> "currentOperation"
    :> ReqBody '[JSON] GimsOperationAnchor
    :> Post '[JSON] GimsCurrentOperationResp

type OperatorTripActionAPI =
  "internal" :> "fleet-operator" :> Capture "gtfs_id" Text :> "tripAction"
    :> ReqBody '[JSON] GimsTripActionReq
    :> Post '[JSON] Value

type OperatorCurrentTripDetailsAPI =
  "internal" :> "fleet-operator" :> Capture "gtfs_id" Text :> "currentTripDetails"
    :> ReqBody '[JSON] GimsCurrentTripDetailsReq
    :> Post '[JSON] GimsCurrentTripDetailsResp

type OperatorActiveTripAPI =
  "internal" :> "fleet-operator" :> Capture "gtfs_id" Text :> "activeTrip"
    :> ReqBody '[JSON] GimsOperationAnchor
    :> Post '[JSON] GimsActiveTripResp

type OperatorEmployeeLoginAPI =
  "internal" :> "fleet-operator" :> Capture "gtfs_id" Text :> "employee" :> "login"
    :> ReqBody '[JSON] GimsEmployeeLoginReq
    :> Post '[JSON] GimsEmployeeLoginResp

type StopChildrenAPI = "station-children" :> Capture "gtfs_id" Text :> Capture "stop_code" Text :> Get '[JSON] [Text]

type StopCodeAPI = "stop-code" :> Capture "gtfs_id" Text :> Capture "provider_stop_code" Text :> Get '[JSON] StopCodeResponse

type OperatorVerifyAPI =
  "internal" :> "fleet-operator" :> Capture "gtfs_id" Text :> "verify"
    :> ReqBody '[JSON] GimsVerifyReq
    :> Post '[JSON] GimsVerifyResp

-- v2 is gtfs_id-scoped: the v1 depot endpoints answer for whichever feed the server loaded, so a
-- wrong feed silently returned data.
type FleetOperationV2API =
  "v2" :> Capture "gtfs_id" Text :> "fleet" :> Capture "fleet_no" Text
    :> Get '[JSON] FleetOperationInfo

type DepotsV2API =
  "v2" :> Capture "gtfs_id" Text :> "depots" :> Get '[JSON] [DepotInfo]

type DepotByIdV2API =
  "v2" :> Capture "gtfs_id" Text :> "depots" :> Capture "depot_id" Text
    :> Get '[JSON] DepotInfo

type DepotVehiclesV2API =
  "v2" :> Capture "gtfs_id" Text :> "depots" :> Capture "depot_id" Text :> "vehicles"
    :> QueryParam "status" Text
    :> Get '[JSON] [DepotVehicleInfo]

-- Every registered bus of the feed (both stores merged), for the fleet-wide live map.
type FleetVehiclesV2API =
  "v2" :> Capture "gtfs_id" Text :> "vehicles"
    :> QueryParam "status" Text
    :> Get '[JSON] [DepotVehicleInfo]

-- One depot's service day: waybills with trips, routes, crew and counts.
type DepotWaybillsV2API =
  "v2" :> Capture "gtfs_id" Text :> "depots" :> Capture "depot_id" Text :> "waybills"
    :> QueryParam' '[Required, Strict] "serviceDate" Day
    :> QueryParam "page" Int
    :> QueryParam "pageSize" Int
    :> QueryParam "q" Text
    :> Get '[JSON] MonitoringAssignments

type DepotRoutesV2API =
  "v2" :> Capture "gtfs_id" Text :> "depots" :> Capture "depot_id" Text :> "routes"
    :> QueryParam' '[Required, Strict] "serviceDate" Day
    :> QueryParam "page" Int
    :> QueryParam "pageSize" Int
    :> QueryParam "q" Text
    :> Get '[JSON] MonitoringRoutes

type DepotCrewV2API =
  "v2" :> Capture "gtfs_id" Text :> "depots" :> Capture "depot_id" Text :> "crew"
    :> QueryParam' '[Required, Strict] "serviceDate" Day
    :> QueryParam "page" Int
    :> QueryParam "pageSize" Int
    :> QueryParam "q" Text
    :> Get '[JSON] MonitoringCrewPage

type DepotOverviewV2API =
  "v2" :> Capture "gtfs_id" Text :> "depots" :> Capture "depot_id" Text :> "overview"
    :> QueryParam' '[Required, Strict] "serviceDate" Day
    :> Get '[JSON] MonitoringOverview

nandiGetRouteStopMappingByRouteIdAPI :: Proxy RouteStopMappingByRouteIdAPI
nandiGetRouteStopMappingByRouteIdAPI = Proxy

nandiGetRouteStopMappingByStopCodeAPI :: Proxy RouteStopMappingByStopCodeAPI
nandiGetRouteStopMappingByStopCodeAPI = Proxy

nandiRouteByRouteIdAPI :: Proxy RouteByRouteIdAPI
nandiRouteByRouteIdAPI = Proxy

nandiExampleTripAPI :: Proxy ExampleTripAPI
nandiExampleTripAPI = Proxy

nandiBusTripScheduleAPI :: Proxy BusTripScheduleAPI
nandiBusTripScheduleAPI = Proxy

operatorCurrentOperationAPI :: Proxy OperatorCurrentOperationAPI
operatorCurrentOperationAPI = Proxy

operatorTripActionAPI :: Proxy OperatorTripActionAPI
operatorTripActionAPI = Proxy

operatorCurrentTripDetailsAPI :: Proxy OperatorCurrentTripDetailsAPI
operatorCurrentTripDetailsAPI = Proxy

operatorActiveTripAPI :: Proxy OperatorActiveTripAPI
operatorActiveTripAPI = Proxy

operatorEmployeeLoginAPI :: Proxy OperatorEmployeeLoginAPI
operatorEmployeeLoginAPI = Proxy

stopChildrenAPI :: Proxy StopChildrenAPI
stopChildrenAPI = Proxy

stopCodeAPI :: Proxy StopCodeAPI
stopCodeAPI = Proxy

operatorVerifyAPI :: Proxy OperatorVerifyAPI
operatorVerifyAPI = Proxy

nandiFleetOperationV2API :: Proxy FleetOperationV2API
nandiFleetOperationV2API = Proxy

nandiDepotsV2API :: Proxy DepotsV2API
nandiDepotsV2API = Proxy

nandiDepotByIdV2API :: Proxy DepotByIdV2API
nandiDepotByIdV2API = Proxy

nandiDepotVehiclesV2API :: Proxy DepotVehiclesV2API
nandiDepotVehiclesV2API = Proxy

nandiFleetVehiclesV2API :: Proxy FleetVehiclesV2API
nandiFleetVehiclesV2API = Proxy

nandiDepotWaybillsV2API :: Proxy DepotWaybillsV2API
nandiDepotWaybillsV2API = Proxy

nandiDepotRoutesV2API :: Proxy DepotRoutesV2API
nandiDepotRoutesV2API = Proxy

nandiDepotCrewV2API :: Proxy DepotCrewV2API
nandiDepotCrewV2API = Proxy

nandiDepotOverviewV2API :: Proxy DepotOverviewV2API
nandiDepotOverviewV2API = Proxy

getNandiGetRouteStopMappingByRouteId :: Text -> Text -> ET.EulerClient [RouteStopMappingInMemoryServer]
getNandiGetRouteStopMappingByRouteId = ET.client nandiGetRouteStopMappingByRouteIdAPI

getNandiGetRouteStopMappingByStopCode :: Text -> Text -> ET.EulerClient [RouteStopMappingInMemoryServer]
getNandiGetRouteStopMappingByStopCode = ET.client nandiGetRouteStopMappingByStopCodeAPI

getNandiRouteByRouteId :: Text -> Text -> ET.EulerClient RouteInfoNandi
getNandiRouteByRouteId = ET.client nandiRouteByRouteIdAPI

getNandiExampleTrip :: Text -> Text -> ET.EulerClient TripDetails
getNandiExampleTrip = ET.client nandiExampleTripAPI

getNandiBusTripSchedule :: Text -> Text -> Int -> Text -> ET.EulerClient BusScheduleDetails
getNandiBusTripSchedule = ET.client nandiBusTripScheduleAPI

postOperatorCurrentOperation :: Text -> GimsOperationAnchor -> ET.EulerClient GimsCurrentOperationResp
postOperatorCurrentOperation = ET.client operatorCurrentOperationAPI

postOperatorTripAction :: Text -> GimsTripActionReq -> ET.EulerClient Value
postOperatorTripAction = ET.client operatorTripActionAPI

postOperatorCurrentTripDetails :: Text -> GimsCurrentTripDetailsReq -> ET.EulerClient GimsCurrentTripDetailsResp
postOperatorCurrentTripDetails = ET.client operatorCurrentTripDetailsAPI

postOperatorActiveTrip :: Text -> GimsOperationAnchor -> ET.EulerClient GimsActiveTripResp
postOperatorActiveTrip = ET.client operatorActiveTripAPI

postOperatorEmployeeLogin :: Text -> GimsEmployeeLoginReq -> ET.EulerClient GimsEmployeeLoginResp
postOperatorEmployeeLogin = ET.client operatorEmployeeLoginAPI

getNandiStopChildren :: Text -> Text -> ET.EulerClient [Text]
getNandiStopChildren = ET.client stopChildrenAPI

getNandiStopCode :: Text -> Text -> ET.EulerClient StopCodeResponse
getNandiStopCode = ET.client stopCodeAPI

postOperatorVerify :: Text -> GimsVerifyReq -> ET.EulerClient GimsVerifyResp
postOperatorVerify = ET.client operatorVerifyAPI

getNandiFleetOperationV2 :: Text -> Text -> ET.EulerClient FleetOperationInfo
getNandiFleetOperationV2 = ET.client nandiFleetOperationV2API

getNandiDepotsV2 :: Text -> ET.EulerClient [DepotInfo]
getNandiDepotsV2 = ET.client nandiDepotsV2API

getNandiDepotByIdV2 :: Text -> Text -> ET.EulerClient DepotInfo
getNandiDepotByIdV2 = ET.client nandiDepotByIdV2API

getNandiDepotVehiclesV2 :: Text -> Text -> Maybe Text -> ET.EulerClient [DepotVehicleInfo]
getNandiDepotVehiclesV2 = ET.client nandiDepotVehiclesV2API

getNandiFleetVehiclesV2 :: Text -> Maybe Text -> ET.EulerClient [DepotVehicleInfo]
getNandiFleetVehiclesV2 = ET.client nandiFleetVehiclesV2API

getNandiDepotWaybillsV2 :: Text -> Text -> Day -> Maybe Int -> Maybe Int -> Maybe Text -> ET.EulerClient MonitoringAssignments
getNandiDepotWaybillsV2 = ET.client nandiDepotWaybillsV2API

getNandiDepotRoutesV2 :: Text -> Text -> Day -> Maybe Int -> Maybe Int -> Maybe Text -> ET.EulerClient MonitoringRoutes
getNandiDepotRoutesV2 = ET.client nandiDepotRoutesV2API

getNandiDepotCrewV2 :: Text -> Text -> Day -> Maybe Int -> Maybe Int -> Maybe Text -> ET.EulerClient MonitoringCrewPage
getNandiDepotCrewV2 = ET.client nandiDepotCrewV2API

getNandiDepotOverviewV2 :: Text -> Text -> Day -> ET.EulerClient MonitoringOverview
getNandiDepotOverviewV2 = ET.client nandiDepotOverviewV2API
