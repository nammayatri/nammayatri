{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform
  ( API,
    handler,
  )
where

import qualified API.RiderPlatform.Booking as Booking
import qualified API.RiderPlatform.Customer as Customer
import qualified API.RiderPlatform.Issue as Issue
import qualified API.RiderPlatform.IssueList as IssueList
import qualified API.RiderPlatform.Merchant as Merchant
import qualified API.RiderPlatform.Ride as Ride
import qualified API.RiderPlatform.RideBooking as RideBooking
import qualified "lib-dashboard" Domain.Types.Merchant as DMerchant
import "lib-dashboard" Environment
import Kernel.Types.Id
import Servant

type API =
  "bap"
    :> Capture "merchantId" (ShortId DMerchant.Merchant)
    :> ( Customer.API
           :<|> Booking.API
           :<|> Merchant.API
           :<|> Ride.API
           :<|> RideBooking.API
           :<|> IssueList.API
           :<|> Issue.API
       )

handler :: FlowServer API
handler merchantId =
  Customer.handler merchantId
    :<|> Booking.handler merchantId
    :<|> Merchant.handler merchantId
    :<|> Ride.handler merchantId
    :<|> RideBooking.handler merchantId
    :<|> IssueList.handler merchantId
    :<|> Issue.handler merchantId
