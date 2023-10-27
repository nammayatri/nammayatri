{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Booking where

import qualified "dashboard-helper-api" Dashboard.Common.Booking as Common
import qualified Domain.Action.Dashboard.Booking as DBooking
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)

type API =
  "booking"
    :> ( Common.StuckBookingsCancelAPI
           :<|> Common.MultipleBookingSyncAPI
       )

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  stuckBookingsCancel merchantId city
    :<|> multipleBookingSync merchantId city

stuckBookingsCancel :: ShortId DM.Merchant -> City.City -> Common.StuckBookingsCancelReq -> FlowHandler Common.StuckBookingsCancelRes
stuckBookingsCancel merchantShortId opCity = withFlowHandlerAPI . DBooking.stuckBookingsCancel merchantShortId opCity

multipleBookingSync :: ShortId DM.Merchant -> City.City -> Common.MultipleBookingSyncReq -> FlowHandler Common.MultipleBookingSyncResp
multipleBookingSync merchantShortId opCity = withFlowHandlerAPI . DBooking.multipleBookingSync merchantShortId opCity
