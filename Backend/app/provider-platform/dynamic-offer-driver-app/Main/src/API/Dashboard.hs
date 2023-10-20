{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard where

import qualified API.Dashboard.Booking as Booking
import qualified API.Dashboard.Driver as Driver
import qualified API.Dashboard.Driver.Registration as DriverRegistration
import qualified API.Dashboard.DriverReferral as DriverReferral
import qualified API.Dashboard.Exotel as Exotel
import qualified API.Dashboard.Fleet.Registration as Fleet
import qualified API.Dashboard.Issue as Issue
import qualified API.Dashboard.Merchant as Merchant
import qualified API.Dashboard.Message as Message
import qualified API.Dashboard.Overlay as Overlay
import qualified API.Dashboard.Revenue as Revenue
import qualified API.Dashboard.Ride as Ride
import qualified API.Dashboard.Scheduler as Scheduler
import qualified API.Dashboard.Subscription as Subscription
import qualified API.Dashboard.Volunteer as Volunteer
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Types.Id
import Servant
import Tools.Auth

type API =
  "dashboard"
    :> ( Capture "merchantId" (ShortId DM.Merchant)
           :> API'
       )
    :<|> ExotelAPI
    :<|> FleetAPI

type API' =
  DashboardTokenAuth
    :> ( Driver.API
           :<|> Ride.API
           :<|> Subscription.API
           :<|> Booking.API
           :<|> Merchant.API
           :<|> Message.API
           :<|> DriverReferral.API
           :<|> DriverRegistration.API
           :<|> Volunteer.API
           :<|> Issue.API
           :<|> Revenue.API
           :<|> Overlay.API
           :<|> Scheduler.API
       )

handler :: FlowServer API
handler =
  ( \merchantId _dashboard ->
      Driver.handler merchantId
        :<|> Ride.handler merchantId
        :<|> Subscription.handler merchantId
        :<|> Booking.handler merchantId
        :<|> Merchant.handler merchantId
        :<|> Message.handler merchantId
        :<|> DriverReferral.handler merchantId
        :<|> DriverRegistration.handler merchantId
        :<|> Volunteer.handler merchantId
        :<|> Issue.handler merchantId
        :<|> Revenue.handler merchantId
        :<|> Overlay.handler merchantId
        :<|> Scheduler.handler merchantId
  )
    :<|> exotelHandler
    :<|> fleetHandler

type ExotelAPI =
  DashboardTokenAuth
    :> Exotel.API

type FleetAPI =
  DashboardTokenAuth
    :> Fleet.API

exotelHandler :: FlowServer ExotelAPI
exotelHandler _dashboard =
  Exotel.handler

fleetHandler :: FlowServer FleetAPI
fleetHandler _dashboard =
  Fleet.handler
