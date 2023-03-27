{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.StaticOfferDriver.Ride
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Ride as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, Money, withFlowHandlerAPI)
import qualified ProviderPlatformClient.StaticOfferDriver as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth hiding (DRIVER_OFFER_BPP)
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "ride"
    :> ( RideListAPI
           :<|> RideStartAPI
           :<|> RideEndAPI
           :<|> RideCancelAPI
           :<|> RideInfoAPI
           :<|> RideSyncAPI
           :<|> RideRouteAPI
       )

type RideListAPI =
  ApiAuth 'BECKN_TRANSPORT 'RIDES 'RIDE_LIST
    :> Common.RideListAPI

type RideRouteAPI =
  ApiAuth 'BECKN_TRANSPORT 'RIDES 'RIDE_ROUTE
    :> Common.RideRouteAPI

type RideStartAPI =
  ApiAuth 'BECKN_TRANSPORT 'RIDES 'RIDE_START
    :> Common.RideStartAPI

type RideEndAPI =
  ApiAuth 'BECKN_TRANSPORT 'RIDES 'RIDE_END
    :> Common.RideEndAPI

type RideCancelAPI =
  ApiAuth 'BECKN_TRANSPORT 'RIDES 'RIDE_CANCEL
    :> Common.RideCancelAPI

type RideInfoAPI =
  ApiAuth 'BECKN_TRANSPORT 'RIDES 'RIDE_INFO
    :> Common.RideInfoAPI

type RideSyncAPI =
  ApiAuth 'BECKN_TRANSPORT 'RIDES 'RIDE_SYNC
    :> Common.RideSyncAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  rideList merchantId
    :<|> rideStart merchantId
    :<|> rideEnd merchantId
    :<|> rideCancel merchantId
    :<|> rideInfo merchantId
    :<|> rideSync merchantId
    :<|> rideRoute merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.RideEndpoint ->
  ApiTokenInfo ->
  Id Common.Ride ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo rideId =
  T.buildTransaction (DT.RideAPI endpoint) (Just BECKN_TRANSPORT) (Just apiTokenInfo) Nothing (Just rideId)

rideList ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  Maybe Money ->
  FlowHandler Common.RideListRes
rideList merchantShortId apiTokenInfo mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone mbFareDiff = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callBecknTransportBPP checkedMerchantId (.rides.rideList) mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone mbFareDiff

rideStart :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> Common.StartRideReq -> FlowHandler APISuccess
rideStart merchantShortId apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideStartEndpoint apiTokenInfo rideId (Just req)
  T.withTransactionStoring transaction $
    Client.callBecknTransportBPP checkedMerchantId (.rides.rideStart) rideId req

rideEnd :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> Common.EndRideReq -> FlowHandler APISuccess
rideEnd merchantShortId apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideEndEndpoint apiTokenInfo rideId (Just req)
  T.withTransactionStoring transaction $
    Client.callBecknTransportBPP checkedMerchantId (.rides.rideEnd) rideId req

rideCancel :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> Common.CancelRideReq -> FlowHandler APISuccess
rideCancel merchantShortId apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideCancelEndpoint apiTokenInfo rideId (Just req)
  T.withTransactionStoring transaction $
    Client.callBecknTransportBPP checkedMerchantId (.rides.rideCancel) rideId req

rideInfo :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> FlowHandler Common.RideInfoRes
rideInfo merchantShortId apiTokenInfo rideId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callBecknTransportBPP checkedMerchantId (.rides.rideInfo) rideId

rideSync :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> FlowHandler Common.RideSyncRes
rideSync merchantShortId apiTokenInfo rideId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideSyncEndpoint apiTokenInfo rideId T.emptyRequest
  T.withResponseTransactionStoring transaction $
    Client.callBecknTransportBPP checkedMerchantId (.rides.rideSync) rideId

rideRoute ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Id Common.Ride ->
  FlowHandler Common.RideRouteRes
rideRoute merchantShortId apiTokenInfo rideId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callBecknTransportBPP checkedMerchantId (.rides.rideRoute) rideId
