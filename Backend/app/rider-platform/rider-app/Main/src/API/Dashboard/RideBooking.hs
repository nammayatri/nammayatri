{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.RideBooking
  ( API,
    handler,
  )
where

import qualified API.Dashboard.RideBooking.Booking as Booking
import qualified API.Dashboard.RideBooking.Confirm as Confirm
import qualified API.Dashboard.RideBooking.Frontend as FlowStatus
import qualified API.Dashboard.RideBooking.Maps as Maps
import qualified API.Dashboard.RideBooking.Profile as Profile
import qualified API.Dashboard.RideBooking.Quote as Quote
import qualified API.Dashboard.RideBooking.Registration as Registration
import qualified API.Dashboard.RideBooking.Search as Search
import qualified API.Dashboard.RideBooking.Select as Select
-- import "lib-dashboard" Environment

import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Types.Id
import Servant

type API =
  "rideBooking"
    :> ( Registration.API
           :<|> Profile.API
           :<|> Search.API
           :<|> Quote.API
           :<|> Select.API
           :<|> Confirm.API
           :<|> Booking.API
           :<|> Maps.API
           :<|> FlowStatus.API
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  Registration.handler merchantId
    :<|> Profile.handler
    :<|> Search.handler
    :<|> Quote.handler
    :<|> Select.handler
    :<|> Confirm.handler
    :<|> Booking.handler
    :<|> Maps.handler
    :<|> FlowStatus.handler
