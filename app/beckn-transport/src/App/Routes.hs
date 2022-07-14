module App.Routes where

import qualified API.Beckn.Handler as Beckn
import qualified API.UI.Driver.Handler as Driver
import qualified API.UI.Registration.Handler as Registration
import qualified API.UI.Ride.Handler as Ride
import qualified API.UI.TranspAdmin.Handler as TranspAdmin
import qualified API.UI.Transporter.Handler as Transporter
import qualified API.UI.Vehicle.Handler as Vehicle
import App.Routes.FarePolicy
import App.Types
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Types.APISuccess
import Beckn.Types.App
import Beckn.Types.Id
import Data.OpenApi
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import EulerHS.Prelude
import qualified Product.Call as Call
import qualified Product.CancellationReason as CancellationReason
import qualified Product.Location as Location
import qualified Product.RideBooking as RideBooking
import qualified Product.Services.GoogleMaps as GoogleMapsFlow
import Servant
import Servant.OpenApi
import qualified Types.API.Call as API
import qualified Types.API.CancellationReason as CancellationReasonAPI
import Types.API.Location as Location
import qualified Types.API.RideBooking as RideBookingAPI
import Utils.Auth (AdminTokenAuth, TokenAuth)

type TransportAPI =
  MainAPI
    :<|> SwaggerAPI

type MainAPI =
  "v2" :> UIAPI
    :<|> "v1" :> OrgBecknAPI

type UIAPI =
  HealthCheckAPI
    :<|> Registration.API
    :<|> TranspAdmin.API
    :<|> Driver.API
    :<|> Vehicle.API
    :<|> Transporter.API
    :<|> RideBookingAPI
    :<|> FarePolicyAPI
    :<|> LocationAPI
    :<|> CallAPIs
    :<|> RouteAPI
    :<|> Ride.API
    :<|> CancellationReasonAPI
    :<|> GoogleMapsProxyAPI

transporterAPI :: Proxy TransportAPI
transporterAPI = Proxy

uiServer :: FlowServer UIAPI
uiServer =
  pure "App is UP"
    :<|> Registration.handler
    :<|> TranspAdmin.handler
    :<|> Driver.handler
    :<|> Vehicle.handler
    :<|> Transporter.handler
    :<|> rideBookingFlow
    :<|> farePolicyFlow
    :<|> locationFlow
    :<|> callFlow
    :<|> routeApiFlow
    :<|> Ride.handler
    :<|> cancellationReasonFlow
    :<|> googleMapsProxyFlow

mainServer :: FlowServer MainAPI
mainServer =
  uiServer
    :<|> orgBecknApiFlow

transporterServer :: FlowServer TransportAPI
transporterServer =
  mainServer
    :<|> writeSwaggerJSONFlow

type RideBookingAPI =
  "org" :> "rideBooking"
    :> ( Capture "bookingId" (Id SRB.RideBooking)
           :> TokenAuth
           :> Post '[JSON] RideBookingAPI.RideBookingStatusRes
           :<|> "list"
             :> AdminTokenAuth
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> QueryParam "onlyActive" Bool
             :> Get '[JSON] RideBookingAPI.RideBookingListRes
           :<|> Capture "bookingId" (Id SRB.RideBooking)
             :> "cancel"
             :> AdminTokenAuth
             :> Get '[JSON] APISuccess
       )
    :<|> "driver"
    :> "rideBooking"
    :> Capture "bookingId" (Id SRB.RideBooking)
    :> "notification"
    :> ( "respond"
           :> TokenAuth
           :> ReqBody '[JSON] RideBookingAPI.SetDriverAcceptanceReq
           :> Post '[JSON] RideBookingAPI.SetDriverAcceptanceRes
           :<|> TokenAuth
           :> Get '[JSON] RideBookingAPI.GetRideInfoRes
       )

rideBookingFlow :: FlowServer RideBookingAPI
rideBookingFlow =
  ( RideBooking.rideBookingStatus
      :<|> RideBooking.rideBookingList
      :<|> RideBooking.rideBookingCancel
  )
    :<|> ( \rideBookingId ->
             RideBooking.setDriverAcceptance rideBookingId
               :<|> RideBooking.getRideInfo rideBookingId
         )

-- Location update and get for tracking is as follows
type LocationAPI =
  "driver" :> "location"
    :> ( Capture "rideId" (Id SRide.Ride) -- TODO: add auth
           :> Get '[JSON] GetLocationRes
           :<|> TokenAuth
           :> ReqBody '[JSON] UpdateLocationReq
           :> Post '[JSON] UpdateLocationRes
       )

locationFlow :: FlowServer LocationAPI
locationFlow =
  Location.getLocation
    :<|> Location.updateLocation

-- location flow over
type OrgBecknAPI = Beckn.API

orgBecknApiFlow :: FlowServer OrgBecknAPI
orgBecknApiFlow = Beckn.handler

-------- Initiate a call (Exotel) APIs --------
type CallAPIs =
  "driver" :> "ride"
    :> Capture "rideId" (Id SRide.Ride)
    :> "call"
    :> ( "rider"
           :> TokenAuth
           :> Post '[JSON] API.CallRes
           :<|> "statusCallback"
           :> ReqBody '[JSON] API.CallCallbackReq
           :> Post '[JSON] API.CallCallbackRes
           :<|> Capture "callId" (Id SCS.CallStatus)
           :> "status"
           :> TokenAuth
           :> Get '[JSON] API.GetCallStatusRes
       )

callFlow :: FlowServer CallAPIs
callFlow rideId =
  Call.initiateCallToCustomer rideId
    :<|> Call.callStatusCallback rideId
    :<|> Call.getCallStatus rideId

type RouteAPI =
  "route"
    :> TokenAuth
    :> ReqBody '[JSON] Location.Request
    :> Post '[JSON] GoogleMaps.DirectionsResp

routeApiFlow :: FlowServer RouteAPI
routeApiFlow = Location.getRoute

type CancellationReasonAPI =
  "cancellationReason"
    :> ( "list"
           :> TokenAuth
           :> Get '[JSON] CancellationReasonAPI.ListRes
       )

cancellationReasonFlow :: FlowServer CancellationReasonAPI
cancellationReasonFlow = CancellationReason.list

type HealthCheckAPI = Get '[JSON] Text

type GoogleMapsProxyAPI =
  "googleMaps"
    :> ( "autoComplete"
           :> TokenAuth
           :> MandatoryQueryParam "input" Text
           :> MandatoryQueryParam "location" Text -- Passing it as <latitude>,<longitude>
           :> MandatoryQueryParam "radius" Integer
           :> MandatoryQueryParam "language" Text
           :> Get '[JSON] GoogleMaps.SearchLocationResp
           :<|> "placeDetails"
             :> TokenAuth
             :> MandatoryQueryParam "place_id" Text
             :> Get '[JSON] GoogleMaps.PlaceDetailsResp
           :<|> "getPlaceName"
             :> TokenAuth
             :> MandatoryQueryParam "latlng" Text -- Passing it as <latitude>,<longitude>
             :> Get '[JSON] GoogleMaps.GetPlaceNameResp
       )

googleMapsProxyFlow :: FlowServer GoogleMapsProxyAPI
googleMapsProxyFlow =
  GoogleMapsFlow.autoComplete
    :<|> GoogleMapsFlow.placeDetails
    :<|> GoogleMapsFlow.getPlaceName

type SwaggerAPI = "swagger" :> Get '[JSON] OpenApi

swagger :: OpenApi
swagger = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Yatri Partner",
            _infoVersion = "1.0"
          }
    }

writeSwaggerJSONFlow :: FlowServer SwaggerAPI
writeSwaggerJSONFlow = return swagger
