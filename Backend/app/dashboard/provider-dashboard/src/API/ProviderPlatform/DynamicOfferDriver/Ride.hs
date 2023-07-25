{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Ride
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
import Kernel.Utils.Common (HighPrecMoney, MonadFlow, withFlowHandlerAPI)
import Kernel.Utils.Validation (runRequestValidation)
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth hiding (BECKN_TRANSPORT)
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "ride"
    :> ( RideListAPI
           :<|> RideStartAPI
           :<|> RideEndAPI
           :<|> MultipleRideEndAPI
           :<|> RideCancelAPI
           :<|> MultipleRideCancelAPI
           :<|> RideInfoAPI
           :<|> RideSyncAPI
           :<|> MultipleRideSyncAPI
           :<|> RideRouteAPI
       )

type RideListAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'RIDE_LIST
    :> Common.RideListAPI

type RideRouteAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'RIDE_ROUTE
    :> Common.RideRouteAPI

type RideStartAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'RIDE_START
    :> Common.RideStartAPI

type RideEndAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'RIDE_END
    :> Common.RideEndAPI

type MultipleRideEndAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'MULTIPLE_RIDE_END
    :> Common.MultipleRideEndAPI

type RideCancelAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'RIDE_CANCEL
    :> Common.RideCancelAPI

type MultipleRideCancelAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'MULTIPLE_RIDE_CANCEL
    :> Common.MultipleRideCancelAPI

type RideInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'RIDE_INFO
    :> Common.RideInfoAPI

type RideSyncAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'RIDE_SYNC
    :> Common.RideSyncAPI

type MultipleRideSyncAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'MULTIPLE_RIDE_SYNC
    :> Common.MultipleRideSyncAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  rideList merchantId
    :<|> rideStart merchantId
    :<|> rideEnd merchantId
    :<|> multipleRideEnd merchantId
    :<|> rideCancel merchantId
    :<|> multipleRideCancel merchantId
    :<|> rideInfo merchantId
    :<|> rideSync merchantId
    :<|> multipleRideSync merchantId
    :<|> rideRoute merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.RideEndpoint ->
  ApiTokenInfo ->
  Maybe (Id Common.Ride) ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.RideAPI endpoint) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) Nothing

rideList ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  Maybe HighPrecMoney ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  FlowHandler Common.RideListRes
rideList merchantShortId apiTokenInfo mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone mbFareDiff mbfrom mbto = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.rides.rideList) mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone mbFareDiff mbfrom mbto

rideStart :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> Common.StartRideReq -> FlowHandler APISuccess
rideStart merchantShortId apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideStartEndpoint apiTokenInfo (Just rideId) (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.rides.rideStart) rideId req

rideEnd :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> Common.EndRideReq -> FlowHandler APISuccess
rideEnd merchantShortId apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideEndEndpoint apiTokenInfo (Just rideId) (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.rides.rideEnd) rideId req

rideCancel :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> Common.CancelRideReq -> FlowHandler APISuccess
rideCancel merchantShortId apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideCancelEndpoint apiTokenInfo (Just rideId) (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.rides.rideCancel) rideId req

rideInfo :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> FlowHandler Common.RideInfoRes
rideInfo merchantShortId apiTokenInfo rideId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.rides.rideInfo) rideId

rideSync :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> FlowHandler Common.RideSyncRes
rideSync merchantShortId apiTokenInfo rideId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideSyncEndpoint apiTokenInfo (Just rideId) T.emptyRequest
  T.withResponseTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.rides.rideSync) rideId

multipleRideSync :: ShortId DM.Merchant -> ApiTokenInfo -> Common.MultipleRideSyncReq -> FlowHandler Common.MultipleRideSyncRes
multipleRideSync merchantShortId apiTokenInfo rideSyncReq = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.MultipleRideSyncEndpoint apiTokenInfo Nothing (Just rideSyncReq)
  T.withResponseTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.rides.multipleRideSync) rideSyncReq

rideRoute ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Id Common.Ride ->
  FlowHandler Common.RideRouteRes
rideRoute merchantShortId apiTokenInfo rideId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.rides.rideRoute) rideId

multipleRideCancel :: ShortId DM.Merchant -> ApiTokenInfo -> Common.MultipleRideCancelReq -> FlowHandler Common.MultipleRideCancelResp
multipleRideCancel merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMultipleRideCancelReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.MultipleRideCancelEndpoint apiTokenInfo Nothing (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.rides.multipleRideCancel) req

multipleRideEnd :: ShortId DM.Merchant -> ApiTokenInfo -> Common.MultipleRideEndReq -> FlowHandler Common.MultipleRideEndResp
multipleRideEnd merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMultipleRideEndReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.MultipleRideEndEndpoint apiTokenInfo Nothing (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.rides.multipleRideEnd) req
