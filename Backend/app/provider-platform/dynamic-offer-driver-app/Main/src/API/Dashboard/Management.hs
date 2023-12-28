{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Management where

import qualified API.Dashboard.Management.Booking as Booking
import qualified API.Dashboard.Management.Driver as Driver
import qualified API.Dashboard.Management.Issue as Issue
import qualified API.Dashboard.Management.Merchant as Merchant
import qualified API.Dashboard.Management.Message as Message
import qualified API.Dashboard.Management.Overlay as Overlay
import qualified API.Dashboard.Management.Revenue as Revenue
import qualified API.Dashboard.Management.Ride as Ride
import qualified API.Dashboard.Management.Subscription as Subscription
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Servant hiding (throwError)
import Tools.Auth

type API =
  DashboardTokenAuth
    :> ( Subscription.API
           :<|> Ride.API
           :<|> Revenue.API
           :<|> Overlay.API
           :<|> Message.API
           :<|> Merchant.API
           :<|> Issue.API
           :<|> Driver.API
           :<|> Booking.API
       )

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city _ = do
  Subscription.handler merchantId city
    :<|> Ride.handler merchantId city
    :<|> Revenue.handler merchantId city
    :<|> Overlay.handler merchantId city
    :<|> Message.handler merchantId city
    :<|> Merchant.handler merchantId city
    :<|> Issue.handler merchantId city
    :<|> Driver.handler merchantId city
    :<|> Booking.handler merchantId city
