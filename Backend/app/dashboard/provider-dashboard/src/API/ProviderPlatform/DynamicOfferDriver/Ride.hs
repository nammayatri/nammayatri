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
import Domain.Types.AccessMatrix.BPP.RideActionType
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, Money, withFlowHandlerAPI)
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth hiding (BECKN_TRANSPORT, CustomerActionType (..), RideActionType (..))
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
  ApiAuth ('DriverOfferBPP ('RIDES 'LIST))
    :> Common.RideListAPI

type RideRouteAPI =
  ApiAuth ('DriverOfferBPP ('RIDES 'LIST))
    :> Common.RideRouteAPI

type RideStartAPI =
  ApiAuth ('DriverOfferBPP ('RIDES 'START))
    :> Common.RideStartAPI

type RideEndAPI =
  ApiAuth ('DriverOfferBPP ('RIDES 'END))
    :> Common.RideEndAPI

type MultipleRideEndAPI =
  ApiAuth ('DriverOfferBPP ('RIDES 'MULTIPLE_RIDE_END))
    :> Common.MultipleRideEndAPI

type RideCancelAPI =
  ApiAuth ('DriverOfferBPP ('RIDES 'CANCEL))
    :> Common.RideCancelAPI

type MultipleRideCancelAPI =
  ApiAuth ('DriverOfferBPP ('RIDES 'MULTIPLE_RIDE_CANCEL))
    :> Common.MultipleRideCancelAPI

type RideInfoAPI =
  ApiAuth ('DriverOfferBPP ('RIDES 'INFO))
    :> Common.RideInfoAPI

type RideSyncAPI =
  ApiAuth ('DriverOfferBPP ('RIDES 'SYNC))
    :> Common.RideSyncAPI

type MultipleRideSyncAPI =
  ApiAuth ('DriverOfferBPP ('RIDES 'MULTIPLE_RIDE_SYNC))
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
buildTransaction endpoint apiTokenInfo rideId =
  T.buildTransaction (DT.RideAPI endpoint) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) Nothing rideId

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

multipleRideEnd :: ShortId DM.Merchant -> ApiTokenInfo -> Common.MultipleRideEndReq -> FlowHandler APISuccess
multipleRideEnd merchantShortId apiTokenInfo rideEndReq = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.MultipleRideEndEndpoint apiTokenInfo Nothing (Just rideEndReq)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.rides.multipleRideEnd) rideEndReq

rideCancel :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> Common.CancelRideReq -> FlowHandler APISuccess
rideCancel merchantShortId apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideCancelEndpoint apiTokenInfo (Just rideId) (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.rides.rideCancel) rideId req

multipleRideCancel :: ShortId DM.Merchant -> ApiTokenInfo -> Common.MultipleRideCancelReq -> FlowHandler APISuccess
multipleRideCancel merchantShortId apiTokenInfo rideEndReq = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.MultipleRideCancelEndpoint apiTokenInfo Nothing (Just rideEndReq)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.rides.multipleRideCancel) rideEndReq

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
