{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI
  ( API,
    handler,
  )
where

import qualified API.UI.Call as Call
import qualified API.UI.CancellationReason as CancellationReason
import qualified API.UI.Driver as Driver
import qualified API.UI.DriverOnboarding as DriverOnboarding
import qualified API.UI.DriverReferral as DriverReferral
import qualified API.UI.FarePolicy as FarePolicy
import qualified API.UI.Frontend as Frontend
import qualified API.UI.Location as Location
import qualified API.UI.Message as Message
import qualified API.UI.OrgAdmin as OrgAdmin
import qualified API.UI.Performance as Performance
import qualified API.UI.Registration as Registration
import qualified API.UI.Ride as Ride
import qualified API.UI.Route as Route
import qualified API.UI.Transporter as Transporter
import qualified API.UI.Vehicle as Vehicle
import qualified API.UI.Whatsapp as Whatsapp
import qualified API.UI.Issue as Issue
import Environment
import Kernel.Prelude
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
           :<|> Whatsapp.API
           :<|> Message.API
           :<|> Performance.API
           :<|> DriverReferral.API
           :<|> Issue.API
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
    :<|> Whatsapp.handler
    :<|> Message.handler
    :<|> Performance.handler
    :<|> DriverReferral.handler
    :<|> Issue.handler
