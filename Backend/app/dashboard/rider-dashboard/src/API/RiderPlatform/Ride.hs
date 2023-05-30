{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.Ride
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as Common
import "lib-dashboard" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Domain.Types.ServerName
import "lib-dashboard" Environment
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import qualified RiderPlatformClient.RiderApp as Client
import Servant
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "ride"
    :> ( ShareRideInfoAPI
           :<|> RideListAPI
           :<|> TripRouteAPI
           :<|> RideInfoAPI
       )

type RideListAPI = Common.RideListAPI

type ShareRideInfoAPI = Common.ShareRideInfoAPI

type TripRouteAPI = Common.TripRouteAPI

type RideInfoAPI =
  ApiAuth 'APP_BACKEND 'CUSTOMERS 'RIDE_INFO_CUSTOMER
    :> Common.RideInfoAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  shareRideInfo merchantId
    :<|> rideList merchantId
    :<|> tripRoute merchantId
    :<|> rideInfo merchantId

rideInfoHitsCountKey :: Id Common.Ride -> Text
rideInfoHitsCountKey rideId = "RideInfoHits:" <> getId rideId <> ":hitsCount"

shareRideInfo ::
  ShortId DM.Merchant ->
  Id Common.Ride ->
  FlowHandler Common.ShareRideInfoRes
shareRideInfo merchantShortId rideId = withFlowHandlerAPI $ do
  shareRideApiRateLimitOptions <- asks (.shareRideApiRateLimitOptions)
  checkSlidingWindowLimitWithOptions (rideInfoHitsCountKey rideId) shareRideApiRateLimitOptions
  checkedMerchantId <- merchantAccessCheck merchantShortId merchantShortId
  Client.callRiderApp checkedMerchantId (.rides.shareRideInfo) rideId

rideList ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  FlowHandler Common.RideListRes
rideList merchantShortId mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone =
  withFlowHandlerAPI $ do
    checkedMerchantId <- merchantAccessCheck merchantShortId merchantShortId
    Client.callRiderApp checkedMerchantId (.rides.rideList) mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone

tripRoute ::
  ShortId DM.Merchant ->
  Id Common.Ride ->
  Double ->
  Double ->
  FlowHandler Maps.GetRoutesResp
tripRoute merchantShortId rideId pickupLocationLat pickupLocationLon = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId merchantShortId
  Client.callRiderApp checkedMerchantId (.rides.tripRoute) rideId pickupLocationLat pickupLocationLon

rideInfo ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Id Common.Ride ->
  FlowHandler Common.RideInfoRes
rideInfo merchantShortId apiTokenInfo rideId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callRiderApp checkedMerchantId (.rides.rideInfo) rideId
