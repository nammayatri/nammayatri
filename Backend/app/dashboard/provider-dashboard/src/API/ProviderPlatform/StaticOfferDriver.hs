{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.StaticOfferDriver
  ( API,
    handler,
  )
where

import qualified API.ProviderPlatform.StaticOfferDriver.Booking as Booking
import qualified API.ProviderPlatform.StaticOfferDriver.Driver as Driver
import qualified API.ProviderPlatform.StaticOfferDriver.Merchant as Merchant
import qualified API.ProviderPlatform.StaticOfferDriver.Ride as Ride
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Kernel.Types.Id
import Servant

type API =
  "beckn-transport"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> ( Driver.API
           :<|> Ride.API
           :<|> Booking.API
           :<|> Merchant.API
       )

handler :: FlowServer API
handler merchantId =
  Driver.handler merchantId
    :<|> Ride.handler merchantId
    :<|> Booking.handler merchantId
    :<|> Merchant.handler merchantId
