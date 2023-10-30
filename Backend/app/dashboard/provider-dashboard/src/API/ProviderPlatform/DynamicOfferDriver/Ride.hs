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
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, Money, withFlowHandlerAPI)
import Kernel.Utils.Validation (runRequestValidation)
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth hiding (BECKN_TRANSPORT, city)
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "ride"
    :> ( RideListAPI
           :<|> RideStartAPI
           :<|> RideEndAPI
           :<|> MultipleRideEndAPI
           :<|> CurrentActiveRideAPI
           :<|> RideCancelAPI
           :<|> MultipleRideCancelAPI
           :<|> RideInfoAPI
           :<|> RideSyncAPI
           :<|> MultipleRideSyncAPI
           :<|> RideRouteAPI
           :<|> BookingWithVehicleNumberAndPhoneAPI
           :<|> TicketRideListAPI
       )

type RideListAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'RIDE_LIST
    :> Common.RideListAPI

type TicketRideListAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'TICKET_RIDE_LIST_API
    :> Common.TicketRideListAPI

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

type CurrentActiveRideAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'CURRENT_ACTIVE_RIDE
    :> Common.CurrentActiveRideAPI

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

type BookingWithVehicleNumberAndPhoneAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'BOOKING_WITH_VEHICLE_NUMBER_AND_PHONE
    :> Common.BookingWithVehicleNumberAndPhoneAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  rideList merchantId city
    :<|> rideStart merchantId city
    :<|> rideEnd merchantId city
    :<|> multipleRideEnd merchantId city
    :<|> currentActiveRide merchantId city
    :<|> rideCancel merchantId city
    :<|> multipleRideCancel merchantId city
    :<|> rideInfo merchantId city
    :<|> rideSync merchantId city
    :<|> multipleRideSync merchantId city
    :<|> rideRoute merchantId city
    :<|> bookingWithVehicleNumberAndPhone merchantId city
    :<|> ticketRideList merchantId city

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
  City.City ->
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
rideList merchantShortId opCity apiTokenInfo mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone mbFareDiff mbfrom mbto = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPP checkedMerchantId opCity (.rides.rideList) mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone mbFareDiff mbfrom mbto

rideStart :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Ride -> Common.StartRideReq -> FlowHandler APISuccess
rideStart merchantShortId opCity apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.RideStartEndpoint apiTokenInfo (Just rideId) (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.rides.rideStart) rideId req

rideEnd :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Ride -> Common.EndRideReq -> FlowHandler APISuccess
rideEnd merchantShortId opCity apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.RideEndEndpoint apiTokenInfo (Just rideId) (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.rides.rideEnd) rideId req

rideCancel :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Ride -> Common.CancelRideReq -> FlowHandler APISuccess
rideCancel merchantShortId opCity apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.RideCancelEndpoint apiTokenInfo (Just rideId) (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.rides.rideCancel) rideId req

rideInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Ride -> FlowHandler Common.RideInfoRes
rideInfo merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPP checkedMerchantId opCity (.rides.rideInfo) rideId

rideSync :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Ride -> FlowHandler Common.RideSyncRes
rideSync merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.RideSyncEndpoint apiTokenInfo (Just rideId) T.emptyRequest
  T.withResponseTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.rides.rideSync) rideId

multipleRideSync :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.MultipleRideSyncReq -> FlowHandler Common.MultipleRideSyncRes
multipleRideSync merchantShortId opCity apiTokenInfo rideSyncReq = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MultipleRideSyncEndpoint apiTokenInfo Nothing (Just rideSyncReq)
  T.withResponseTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.rides.multipleRideSync) rideSyncReq

rideRoute ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Id Common.Ride ->
  FlowHandler Common.RideRouteRes
rideRoute merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPP checkedMerchantId opCity (.rides.rideRoute) rideId

multipleRideCancel :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.MultipleRideCancelReq -> FlowHandler Common.MultipleRideCancelResp
multipleRideCancel merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMultipleRideCancelReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MultipleRideCancelEndpoint apiTokenInfo Nothing (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.rides.multipleRideCancel) req

multipleRideEnd :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.MultipleRideEndReq -> FlowHandler Common.MultipleRideEndResp
multipleRideEnd merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMultipleRideEndReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MultipleRideEndEndpoint apiTokenInfo Nothing (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.rides.multipleRideEnd) req

currentActiveRide :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> FlowHandler (Id Common.Ride)
currentActiveRide merchantShortId opCity apiTokenInfo vehichleNumber = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPP checkedMerchantId opCity (.rides.currentActiveRide) vehichleNumber

bookingWithVehicleNumberAndPhone :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.BookingWithVehicleAndPhoneReq -> FlowHandler Common.BookingWithVehicleAndPhoneRes
bookingWithVehicleNumberAndPhone merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.BookingWithVehicleNumberAndPhoneEndpoint apiTokenInfo Nothing (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.rides.bookingWithVehicleNumberAndPhone) req

ticketRideList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe (ShortId Common.Ride) -> Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler Common.TicketRideListRes
ticketRideList merchantShortId opCity apiTokenInfo mbRideShortId mbCountryCode mbPhoneNumber mbSupportPhoneNumber = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.TicketRideListEndpoint apiTokenInfo Nothing T.emptyRequest
  T.withResponseTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.rides.ticketRideList) mbRideShortId mbCountryCode mbPhoneNumber mbSupportPhoneNumber
