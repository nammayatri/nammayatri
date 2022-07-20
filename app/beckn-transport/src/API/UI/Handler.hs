module API.UI.Handler (API, handler) where

import qualified API.UI.Booking.Handler as Booking
import qualified API.UI.Call.Handler as Call
import qualified API.UI.CancellationReason.Handler as CancellationReason
import qualified API.UI.Driver.Handler as Driver
import qualified API.UI.FarePolicy.Handler as FarePolicy
import qualified API.UI.GoogleMaps.Handler as GoogleMaps
import qualified API.UI.Location.Handler as Location
import qualified API.UI.Registration.Handler as Registration
import qualified API.UI.Ride.Handler as Ride
import qualified API.UI.Route.Handler as Route
import qualified API.UI.TranspAdmin.Handler as TranspAdmin
import qualified API.UI.Transporter.Handler as Transporter
import qualified API.UI.Vehicle.Handler as Vehicle
import App.Types
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
           :<|> Call.API
           :<|> Route.API
           :<|> Ride.API
           :<|> CancellationReason.API
           :<|> GoogleMaps.API
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
    :<|> Call.handler
    :<|> Route.handler
    :<|> Ride.handler
    :<|> CancellationReason.handler
    :<|> GoogleMaps.handler

type HealthCheckAPI = Get '[JSON] Text
