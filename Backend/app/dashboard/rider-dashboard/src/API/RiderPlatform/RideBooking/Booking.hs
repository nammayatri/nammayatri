{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.RideBooking.Booking where

import qualified "rider-app" API.Dashboard.RideBooking.Booking as BAP
import qualified Dashboard.RiderPlatform.Customer as Common
import qualified "rider-app" Domain.Action.UI.Booking as DBooking
import qualified "rider-app" Domain.Types.Booking as SRB
import qualified "rider-app" Domain.Types.Booking.API as DB
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "rider-app" Domain.Types.Person as DP
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp.RideBooking as Client
import Servant
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "booking"
    :> ( ApiAuth 'APP_BACKEND 'CUSTOMERS 'BOOKING_STATUS
           :> BAP.CustomerBookingStatusAPI
           :<|> ApiAuth 'APP_BACKEND 'CUSTOMERS 'BOOKINGLIST
             :> BAP.CustomerBookingListAPI
       )

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  callBookingStatus merchantId city
    :<|> callBookingList merchantId city

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  BAP.RideBookingEndPoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.RBooking endpoint) (Just APP_BACKEND) (Just apiTokenInfo) Nothing Nothing

callBookingStatus :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id SRB.Booking -> Id DP.Person -> FlowHandler DB.BookingAPIEntity
callBookingStatus merchantShortId opCity apiTokenInfo bookingId personId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction BAP.RideStatusEndPoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId opCity (.rideBooking.booking.bookingStatus) bookingId personId

callBookingList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id DP.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> FlowHandler DBooking.BookingListRes
callBookingList merchantShortId opCity apiTokenInfo personId limit offset onlyActive status = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderApp checkedMerchantId opCity (.rideBooking.booking.bookingList) personId limit offset onlyActive status
