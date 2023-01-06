module API.UI (API, handler) where

import qualified API.UI.Booking as Booking
import qualified API.UI.Call as Call
import qualified API.UI.CancellationReason as CancellationReason
import qualified API.UI.Driver as Driver
import qualified API.UI.FarePolicy as FarePolicy
import qualified API.UI.GoogleMaps as GoogleMaps
import qualified API.UI.Location as Location
import qualified API.UI.Registration as Registration
import qualified API.UI.Ride as Ride
import qualified API.UI.Route as Route
import qualified API.UI.TranspAdmin as TranspAdmin
import qualified API.UI.Transporter as Transporter
import qualified API.UI.Vehicle as Vehicle
import qualified API.UI.Webengage.InfoBIPWebhook as InfoBIPWebhook
import qualified API.UI.Webengage.WebengageSms as Webengage
import Environment
import EulerHS.Prelude
import Servant

type API =
  "v2"
    :> ( HealthCheckAPI
           :<|> Registration.API
           :<|> TranspAdmin.API
           :<|> Driver.API
           :<|> Vehicle.API
           :<|> Transporter.API
           :<|> Booking.API
           :<|> FarePolicy.API
           :<|> Location.API
           :<|> Call.DeprecatedAPI
           :<|> Call.API
           :<|> Route.API
           :<|> Ride.API
           :<|> CancellationReason.API
           :<|> GoogleMaps.API
           :<|> Webengage.API
           :<|> InfoBIPWebhook.API
       )

handler :: FlowServer API
handler =
  pure "App is UP"
    :<|> Registration.handler
    :<|> TranspAdmin.handler
    :<|> Driver.handler
    :<|> Vehicle.handler
    :<|> Transporter.handler
    :<|> Booking.handler
    :<|> FarePolicy.handler
    :<|> Location.handler
    :<|> Call.deprecatedHandler
    :<|> Call.handler
    :<|> Route.handler
    :<|> Ride.handler
    :<|> CancellationReason.handler
    :<|> GoogleMaps.handler
    :<|> Webengage.handler
    :<|> InfoBIPWebhook.handler

type HealthCheckAPI = Get '[JSON] Text
