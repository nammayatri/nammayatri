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
import qualified API.UI.CallEvent as CallEvent
import qualified API.UI.CancellationReason as CancellationReason
import qualified API.UI.City as City
import qualified API.UI.Driver as Driver
import qualified API.UI.DriverCoins as DriverCoins
import qualified API.UI.DriverOnboarding as DriverOnboarding
import qualified API.UI.DriverProfileSummary as DriverProfileSummary
import qualified API.UI.DriverReferral as DriverReferral
import qualified API.UI.ExotelEndRide as ExotelEndRide
import qualified API.UI.Issue as Issue
import qualified API.UI.KioskLocation as KioskLocation
import qualified API.UI.LeaderBoard as LeaderBoard
import qualified API.UI.Maps as Maps
import qualified API.UI.Message as Message
import qualified API.UI.OnMessage as OnMessage
import qualified API.UI.OrgAdmin as OrgAdmin
import qualified API.UI.Payment as Payment
import qualified API.UI.Performance as Performance
import qualified API.UI.Plan as Plan
import qualified API.UI.Rating as Rating
import qualified API.UI.Registration as Registration
import qualified API.UI.Ride as Ride
import qualified API.UI.RideRoute as RideRoute
import qualified API.UI.RideSummary as RideSummary
import qualified API.UI.Route as Route
import qualified API.UI.Transporter as Transporter
import qualified API.UI.Vehicle as Vehicle
import qualified API.UI.Whatsapp as Whatsapp
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
           :<|> Payment.API
           :<|> Payment.PaymentCustomerAPI
           :<|> Driver.API
           :<|> DriverProfileSummary.API
           :<|> Vehicle.API
           :<|> Transporter.API
           :<|> Route.API
           :<|> Maps.API
           :<|> Ride.API
           :<|> Call.API
           :<|> CancellationReason.API
           :<|> Whatsapp.API
           :<|> Message.API
           :<|> Performance.API
           :<|> Rating.API
           :<|> DriverReferral.API
           :<|> Issue.API
           :<|> ExotelEndRide.API
           :<|> LeaderBoard.API
           :<|> OnMessage.API
           :<|> RideRoute.API
           :<|> CallEvent.API
           :<|> Plan.API
           :<|> KioskLocation.API
           :<|> DriverCoins.API
           :<|> RideSummary.API
           :<|> City.API
       )

handler :: FlowServer API
handler =
  pure "App is UP"
    :<|> Registration.handler
    :<|> DriverOnboarding.handler
    :<|> OrgAdmin.handler
    :<|> Payment.handler
    :<|> Payment.handler2
    :<|> Driver.handler
    :<|> DriverProfileSummary.handler
    :<|> Vehicle.handler
    :<|> Transporter.handler
    :<|> Route.handler
    :<|> Maps.handler
    :<|> Ride.handler
    :<|> Call.handler
    :<|> CancellationReason.handler
    :<|> Whatsapp.handler
    :<|> Message.handler
    :<|> Performance.handler
    :<|> Rating.handler
    :<|> DriverReferral.handler
    :<|> Issue.handler
    :<|> ExotelEndRide.handler
    :<|> LeaderBoard.handler
    :<|> OnMessage.handler
    :<|> RideRoute.handler
    :<|> CallEvent.handler
    :<|> Plan.handler
    :<|> KioskLocation.handler
    :<|> DriverCoins.handler
    :<|> RideSummary.handler
    :<|> City.handler
