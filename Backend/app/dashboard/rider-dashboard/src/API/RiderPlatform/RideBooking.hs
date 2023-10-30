{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.RideBooking
  ( API,
    handler,
  )
where

import qualified API.RiderPlatform.RideBooking.Booking as Booking
import qualified API.RiderPlatform.RideBooking.Cancel as Cancel
import qualified API.RiderPlatform.RideBooking.Confirm as Confirm
import qualified API.RiderPlatform.RideBooking.Frontend as FlowStatus
import qualified API.RiderPlatform.RideBooking.Maps as Maps
import qualified API.RiderPlatform.RideBooking.Profile as Profile
import qualified API.RiderPlatform.RideBooking.Quote as Quote
import qualified API.RiderPlatform.RideBooking.Registration as Registration
import qualified API.RiderPlatform.RideBooking.Search as Search
import qualified API.RiderPlatform.RideBooking.Select as Select
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Kernel.Types.Beckn.City as City
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
           :<|> Cancel.API
       )

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  Registration.handler merchantId city
    :<|> Profile.handler merchantId city
    :<|> Search.handler merchantId city
    :<|> Quote.handler merchantId city
    :<|> Select.handler merchantId city
    :<|> Confirm.handler merchantId city
    :<|> Booking.handler merchantId city
    :<|> Maps.handler merchantId city
    :<|> FlowStatus.handler merchantId city
    :<|> Cancel.handler merchantId city
