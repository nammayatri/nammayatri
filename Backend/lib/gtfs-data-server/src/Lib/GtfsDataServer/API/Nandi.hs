module Lib.GtfsDataServer.API.Nandi where

import Data.Aeson
import qualified EulerHS.Types as ET
import Kernel.Prelude
import Lib.GtfsDataServer.Types
import Servant

type RouteStopMappingByRouteIdAPI = "route-stop-mapping" :> Capture "gtfs_id" Text :> "route" :> Capture "route_code" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

type RouteStopMappingByStopCodeAPI = "route-stop-mapping" :> Capture "gtfs_id" Text :> "stop" :> Capture "stop_code" Text :> Get '[JSON] [RouteStopMappingInMemoryServer]

type RouteByRouteIdAPI = "route" :> Capture "gtfs_id" Text :> Capture "route_id" Text :> Get '[JSON] RouteInfoNandi

type ExampleTripAPI = "example-trip" :> Capture "gtfs_id" Text :> Capture "route_id" Text :> Get '[JSON] TripDetails

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

nandiGetRouteStopMappingByRouteIdAPI :: Proxy RouteStopMappingByRouteIdAPI
nandiGetRouteStopMappingByRouteIdAPI = Proxy

nandiGetRouteStopMappingByStopCodeAPI :: Proxy RouteStopMappingByStopCodeAPI
nandiGetRouteStopMappingByStopCodeAPI = Proxy

nandiRouteByRouteIdAPI :: Proxy RouteByRouteIdAPI
nandiRouteByRouteIdAPI = Proxy

nandiExampleTripAPI :: Proxy ExampleTripAPI
nandiExampleTripAPI = Proxy

operatorCurrentOperationAPI :: Proxy OperatorCurrentOperationAPI
operatorCurrentOperationAPI = Proxy

operatorTripActionAPI :: Proxy OperatorTripActionAPI
operatorTripActionAPI = Proxy

operatorCurrentTripDetailsAPI :: Proxy OperatorCurrentTripDetailsAPI
operatorCurrentTripDetailsAPI = Proxy

operatorEmployeeLoginAPI :: Proxy OperatorEmployeeLoginAPI
operatorEmployeeLoginAPI = Proxy

stopChildrenAPI :: Proxy StopChildrenAPI
stopChildrenAPI = Proxy

stopCodeAPI :: Proxy StopCodeAPI
stopCodeAPI = Proxy

operatorVerifyAPI :: Proxy OperatorVerifyAPI
operatorVerifyAPI = Proxy

getNandiGetRouteStopMappingByRouteId :: Text -> Text -> ET.EulerClient [RouteStopMappingInMemoryServer]
getNandiGetRouteStopMappingByRouteId = ET.client nandiGetRouteStopMappingByRouteIdAPI

getNandiGetRouteStopMappingByStopCode :: Text -> Text -> ET.EulerClient [RouteStopMappingInMemoryServer]
getNandiGetRouteStopMappingByStopCode = ET.client nandiGetRouteStopMappingByStopCodeAPI

getNandiRouteByRouteId :: Text -> Text -> ET.EulerClient RouteInfoNandi
getNandiRouteByRouteId = ET.client nandiRouteByRouteIdAPI

getNandiExampleTrip :: Text -> Text -> ET.EulerClient TripDetails
getNandiExampleTrip = ET.client nandiExampleTripAPI

postOperatorCurrentOperation :: Text -> GimsOperationAnchor -> ET.EulerClient GimsCurrentOperationResp
postOperatorCurrentOperation = ET.client operatorCurrentOperationAPI

postOperatorTripAction :: Text -> GimsTripActionReq -> ET.EulerClient Value
postOperatorTripAction = ET.client operatorTripActionAPI

postOperatorCurrentTripDetails :: Text -> GimsCurrentTripDetailsReq -> ET.EulerClient GimsCurrentTripDetailsResp
postOperatorCurrentTripDetails = ET.client operatorCurrentTripDetailsAPI

postOperatorEmployeeLogin :: Text -> GimsEmployeeLoginReq -> ET.EulerClient GimsEmployeeLoginResp
postOperatorEmployeeLogin = ET.client operatorEmployeeLoginAPI

getNandiStopChildren :: Text -> Text -> ET.EulerClient [Text]
getNandiStopChildren = ET.client stopChildrenAPI

getNandiStopCode :: Text -> Text -> ET.EulerClient StopCodeResponse
getNandiStopCode = ET.client stopCodeAPI

postOperatorVerify :: Text -> GimsVerifyReq -> ET.EulerClient GimsVerifyResp
postOperatorVerify = ET.client operatorVerifyAPI
