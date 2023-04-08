{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Volunteer where

import API.Dashboard.Driver (listDrivers)
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Volunteer as Common
import qualified Domain.Action.Dashboard.Volunteer as DVolunteer
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (throwError)

type API =
  "volunteer"
    :> ( Common.DriverListAPI
           :<|> Common.BookingInfoAPI
           :<|> Common.AssignCreateAndStartOtpRideAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listDrivers merchantId
    :<|> bookingInfo merchantId
    :<|> assignCreateAndStartOtpRide merchantId

bookingInfo :: ShortId DM.Merchant -> Text -> FlowHandler Common.BookingInfoResponse
bookingInfo merchantShortId =
  withFlowHandlerAPI . DVolunteer.bookingInfo merchantShortId

assignCreateAndStartOtpRide :: ShortId DM.Merchant -> Common.AssignCreateAndStartOtpRideAPIReq -> FlowHandler APISuccess
assignCreateAndStartOtpRide merchantShortId = withFlowHandlerAPI . DVolunteer.assignCreateAndStartOtpRide merchantShortId
