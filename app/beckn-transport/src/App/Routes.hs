module App.Routes where

import qualified API.Beckn.Handler as Beckn
import qualified API.UI.Booking.Handler as Booking
import qualified API.UI.Call.Handler as Call
import qualified API.UI.CancellationReason.Handler as CancellationReason
import qualified API.UI.Driver.Handler as Driver
import qualified API.UI.Location.Handler as Location
import qualified API.UI.Registration.Handler as Registration
import qualified API.UI.Ride.Handler as Ride
import qualified API.UI.Route.Handler as Route
import qualified API.UI.TranspAdmin.Handler as TranspAdmin
import qualified API.UI.Transporter.Handler as Transporter
import qualified API.UI.Vehicle.Handler as Vehicle
import App.Routes.FarePolicy
import App.Types
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Types.App
import Data.OpenApi
import EulerHS.Prelude
import qualified Product.Services.GoogleMaps as GoogleMapsFlow
import Servant
import Servant.OpenApi
import Utils.Auth (TokenAuth)

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
    :<|> Booking.API
    :<|> FarePolicyAPI
    :<|> Location.API
    :<|> Call.API
    :<|> Route.API
    :<|> Ride.API
    :<|> CancellationReason.API
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
    :<|> Booking.handler
    :<|> farePolicyFlow
    :<|> Location.handler
    :<|> Call.handler
    :<|> Route.handler
    :<|> Ride.handler
    :<|> CancellationReason.handler
    :<|> googleMapsProxyFlow

mainServer :: FlowServer MainAPI
mainServer =
  uiServer
    :<|> orgBecknApiFlow

transporterServer :: FlowServer TransportAPI
transporterServer =
  mainServer
    :<|> writeSwaggerJSONFlow

-- location flow over
type OrgBecknAPI = Beckn.API

orgBecknApiFlow :: FlowServer OrgBecknAPI
orgBecknApiFlow = Beckn.handler

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
