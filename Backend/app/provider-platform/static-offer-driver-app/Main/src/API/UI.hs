{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI (API, handler) where

import qualified API.UI.Booking as Booking
import qualified API.UI.Call as Call
import qualified API.UI.CancellationReason as CancellationReason
import qualified API.UI.Driver as Driver
import qualified API.UI.FarePolicy as FarePolicy
import qualified API.UI.Location as Location
import qualified API.UI.Maps as Maps
import qualified API.UI.Registration as Registration
import qualified API.UI.Ride as Ride
import qualified API.UI.Route as Route
import qualified API.UI.TranspAdmin as TranspAdmin
import qualified API.UI.Transporter as Transporter
import qualified API.UI.Vehicle as Vehicle
import qualified API.UI.Webengage.InfoBIPWebhook as InfoBIPWebhook
import qualified API.UI.Webengage.Webengage as Webengage
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
           :<|> Call.API
           :<|> Route.API
           :<|> Ride.API
           :<|> CancellationReason.API
           :<|> Maps.API
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
    :<|> Call.handler
    :<|> Route.handler
    :<|> Ride.handler
    :<|> CancellationReason.handler
    :<|> Maps.handler
    :<|> Webengage.handler
    :<|> InfoBIPWebhook.handler

type HealthCheckAPI = Get '[JSON] Text
