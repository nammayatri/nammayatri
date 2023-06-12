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

import qualified "rider-app" API.Dashboard.Ride as BAP
import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as Common
import qualified "rider-app" Domain.Action.Dashboard.Ride as Domain
import "lib-dashboard" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Domain.Types.ServerName
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import qualified RiderPlatformClient.RiderApp as Client
import Servant
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth hiding (BECKN_TRANSPORT, DRIVER_OFFER_BPP)
import Tools.Auth.Merchant

type API =
  "ride"
    :> ( ShareRideInfoAPI
           :<|> RideListAPI
           :<|> TripRouteAPI
           :<|> RideInfoAPI
           :<|> MultipleRideCancelAPI
       )

type RideListAPI = Common.RideListAPI

type ShareRideInfoAPI = Common.ShareRideInfoAPI

type TripRouteAPI = Common.TripRouteAPI

type RideInfoAPI =
  ApiAuth 'APP_BACKEND 'CUSTOMERS 'RIDE_INFO_CUSTOMER
    :> Common.RideInfoAPI

type MultipleRideCancelAPI =
  ApiAuth 'APP_BACKEND 'RIDES 'MULTIPLE_RIDE_CANCEL
    :> BAP.MultipleRideCancelAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  shareRideInfo merchantId
    :<|> rideList merchantId
    :<|> tripRoute merchantId
    :<|> rideInfo merchantId
    :<|> multipleRideCancel merchantId

rideInfoHitsCountKey :: Id Common.Ride -> Text
rideInfoHitsCountKey rideId = "RideInfoHits:" <> getId rideId <> ":hitsCount"

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.RideEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.RideAPI endpoint) (Just APP_BACKEND) (Just apiTokenInfo) Nothing Nothing

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
  Maybe UTCTime ->
  Maybe UTCTime ->
  FlowHandler Common.RideListRes
rideList merchantShortId mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone mbFrom mbTo =
  withFlowHandlerAPI $ do
    checkedMerchantId <- merchantAccessCheck merchantShortId merchantShortId
    Client.callRiderApp checkedMerchantId (.rides.rideList) mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone mbFrom mbTo

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

multipleRideCancel :: ShortId DM.Merchant -> ApiTokenInfo -> Domain.MultipleRideCancelReq -> FlowHandler APISuccess
multipleRideCancel merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.MultipleRideCancelEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.rides.multipleRideCancel) req
