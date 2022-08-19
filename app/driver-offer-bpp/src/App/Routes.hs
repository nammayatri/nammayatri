module App.Routes where

import qualified API.Beckn as Beckn
import qualified API.UI.Call as Call
import qualified API.UI.CancellationReason as CancellationReason
import qualified API.UI.Driver as Driver
import qualified API.UI.DriverOnboarding as DriverOnboarding
import qualified API.UI.FarePolicy as FarePolicy
import qualified API.UI.Location as Location
import qualified API.UI.OrgAdmin as OrgAdmin
import qualified API.UI.Registration as Registration
import qualified API.UI.Ride as Ride
import qualified API.UI.Route as Route
import qualified API.UI.Transporter as Transporter
import qualified API.UI.Vehicle as Vehicle
import Data.OpenApi
import Environment
import EulerHS.Prelude
import Servant
import Servant.OpenApi

type DriverOfferAPI =
  MainAPI
    :<|> SwaggerAPI

type MainAPI =
  UIAPI
    :<|> Beckn.API

type UIAPI =
  "ui"
    :> ( HealthCheckAPI
           :<|> Registration.API
           :<|> DriverOnboarding.API
           :<|> OrgAdmin.API
           :<|> Driver.API
           :<|> Vehicle.API
           :<|> Transporter.API
           :<|> FarePolicy.API
           :<|> Location.API
           :<|> Route.API
           :<|> Ride.API
           :<|> Call.API
           :<|> CancellationReason.API
       )

driverOfferAPI :: Proxy DriverOfferAPI
driverOfferAPI = Proxy

uiServer :: FlowServer UIAPI
uiServer =
  pure "App is UP"
    :<|> Registration.handler
    :<|> DriverOnboarding.handler
    :<|> OrgAdmin.handler
    :<|> Driver.handler
    :<|> Vehicle.handler
    :<|> Transporter.handler
    :<|> FarePolicy.handler
    :<|> Location.handler
    :<|> Route.handler
    :<|> Ride.handler
    :<|> Call.handler
    :<|> CancellationReason.handler

mainServer :: FlowServer MainAPI
mainServer =
  uiServer
    :<|> Beckn.handler

driverOfferServer :: FlowServer DriverOfferAPI
driverOfferServer =
  mainServer
    :<|> writeSwaggerJSONFlow

type HealthCheckAPI = Get '[JSON] Text

type SwaggerAPI = "swagger" :> Get '[JSON] OpenApi

swagger :: OpenApi
swagger = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Namma Yatri Partner",
            _infoVersion = "1.0"
          }
    }

writeSwaggerJSONFlow :: FlowServer SwaggerAPI
writeSwaggerJSONFlow = return swagger
