{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Volunteer where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Volunteer as Common
import qualified Domain.Action.Dashboard.Volunteer as DVolunteer
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (throwError)

type API =
  "volunteer"
    :> ( Common.BookingInfoAPI
           :<|> Common.AssignCreateAndStartOtpRideAPI
       )

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  bookingInfo merchantId city
    :<|> assignCreateAndStartOtpRide merchantId city

bookingInfo :: ShortId DM.Merchant -> City.City -> Text -> FlowHandler Common.BookingInfoResponse
bookingInfo merchantShortId opCity =
  withFlowHandlerAPI . DVolunteer.bookingInfo merchantShortId opCity

assignCreateAndStartOtpRide :: ShortId DM.Merchant -> City.City -> Common.AssignCreateAndStartOtpRideAPIReq -> FlowHandler APISuccess
assignCreateAndStartOtpRide merchantShortId opCity = withFlowHandlerAPI . DVolunteer.assignCreateAndStartOtpRide merchantShortId opCity
