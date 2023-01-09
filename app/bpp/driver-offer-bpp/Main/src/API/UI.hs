module API.UI
  ( API,
    handler,
  )
where

import qualified API.UI.Call as Call
import qualified API.UI.CancellationReason as CancellationReason
import qualified API.UI.Driver as Driver
import qualified API.UI.DriverOnboarding as DriverOnboarding
import qualified API.UI.FarePolicy as FarePolicy
import qualified API.UI.Frontend as Frontend
import qualified API.UI.Location as Location
import qualified API.UI.OrgAdmin as OrgAdmin
import qualified API.UI.Registration as Registration
import qualified API.UI.Ride as Ride
import qualified API.UI.Route as Route
import qualified API.UI.Transporter as Transporter
import qualified API.UI.Vehicle as Vehicle
import Beckn.Prelude
import Environment
import Servant

type HealthCheckAPI = Get '[JSON] Text

type API =
  "ui"
    :> ( HealthCheckAPI
           :<|> Registration.API
           :<|> DriverOnboarding.API
           :<|> OrgAdmin.API
           :<|> Driver.API
           :<|> Vehicle.API
           :<|> Frontend.API
           :<|> Transporter.API
           :<|> FarePolicy.API
           :<|> Location.API
           :<|> Route.API
           :<|> Ride.API
           :<|> Call.API
           :<|> CancellationReason.API
       )

handler :: FlowServer API
handler =
  pure "App is UP"
    :<|> Registration.handler
    :<|> DriverOnboarding.handler
    :<|> OrgAdmin.handler
    :<|> Driver.handler
    :<|> Vehicle.handler
    :<|> Frontend.handler
    :<|> Transporter.handler
    :<|> FarePolicy.handler
    :<|> Location.handler
    :<|> Route.handler
    :<|> Ride.handler
    :<|> Call.handler
    :<|> CancellationReason.handler
