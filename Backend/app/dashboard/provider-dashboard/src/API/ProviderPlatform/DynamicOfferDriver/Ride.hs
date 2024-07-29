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

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.Ride as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI')
import qualified ProviderPlatformClient.DynamicOfferDriver.RideBooking as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "ride"
    :> ( RideStartAPI
           :<|> RideEndAPI
           :<|> CurrentActiveRideAPI
           :<|> RideCancelAPI
           :<|> BookingWithVehicleNumberAndPhoneAPI
           :<|> FareBreakUpAPI
       )

type RideStartAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'RIDE_START
    :> Common.RideStartAPI

type RideEndAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'RIDE_END
    :> Common.RideEndAPI

type CurrentActiveRideAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'CURRENT_ACTIVE_RIDE
    :> Common.CurrentActiveRideAPI

type RideCancelAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'RIDE_CANCEL
    :> Common.RideCancelAPI

type BookingWithVehicleNumberAndPhoneAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'RIDES 'BOOKING_WITH_VEHICLE_NUMBER_AND_PHONE
    :> Common.BookingWithVehicleNumberAndPhoneAPI

type FareBreakUpAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'RIDES 'FARE_BREAKUP
    :> Common.FareBreakUpAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  rideStart merchantId city
    :<|> rideEnd merchantId city
    :<|> currentActiveRide merchantId city
    :<|> rideCancel merchantId city
    :<|> bookingWithVehicleNumberAndPhone merchantId city
    :<|> fareBreakUp merchantId city

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

rideStart :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Ride -> Common.StartRideReq -> FlowHandler APISuccess
rideStart merchantShortId opCity apiTokenInfo rideId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.RideStartEndpoint apiTokenInfo (Just rideId) (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.rides.rideStart) rideId req

rideEnd :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Ride -> Common.EndRideReq -> FlowHandler APISuccess
rideEnd merchantShortId opCity apiTokenInfo rideId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.RideEndEndpoint apiTokenInfo (Just rideId) (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.rides.rideEnd) rideId req

rideCancel :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Ride -> Common.CancelRideReq -> FlowHandler APISuccess
rideCancel merchantShortId opCity apiTokenInfo rideId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.RideCancelEndpoint apiTokenInfo (Just rideId) (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.rides.rideCancel) rideId req

currentActiveRide :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> FlowHandler (Id Common.Ride)
currentActiveRide merchantShortId opCity apiTokenInfo vehichleNumber = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPP checkedMerchantId opCity (.rides.currentActiveRide) vehichleNumber

bookingWithVehicleNumberAndPhone :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.BookingWithVehicleAndPhoneReq -> FlowHandler Common.BookingWithVehicleAndPhoneRes
bookingWithVehicleNumberAndPhone merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.BookingWithVehicleNumberAndPhoneEndpoint apiTokenInfo Nothing (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId opCity (.rides.bookingWithVehicleNumberAndPhone) req

fareBreakUp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Ride -> FlowHandler Common.FareBreakUpRes
fareBreakUp merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPP checkedMerchantId opCity (.rides.fareBreakUp) rideId
