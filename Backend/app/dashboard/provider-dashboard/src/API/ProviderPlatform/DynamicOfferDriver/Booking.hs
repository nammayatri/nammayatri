{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Booking
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.Common.Booking as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI')
import Kernel.Utils.Validation (runRequestValidation)
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "booking"
    :> ( StuckBookingsCancelAPI
           :<|> MultipleBookingSyncAPI
       )

type StuckBookingsCancelAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'RIDES 'STUCK_BOOKING_CANCEL
    :> Common.StuckBookingsCancelAPI

type MultipleBookingSyncAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'RIDES 'MULTIPLE_BOOKING_SYNC
    :> Common.MultipleBookingSyncAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  stuckBookingsCancel merchantId city
    :<|> multipleBookingSync merchantId city

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.BookingEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.BookingAPI endpoint) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

stuckBookingsCancel :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.StuckBookingsCancelReq -> FlowHandler Common.StuckBookingsCancelRes
stuckBookingsCancel merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.StuckBookingsCancelEndpoint apiTokenInfo (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.bookings.stuckBookingsCancel) req

multipleBookingSync :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.MultipleBookingSyncReq -> FlowHandler Common.MultipleBookingSyncResp
multipleBookingSync merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateMultipleBookingSyncReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MultipleBookingSyncEndpoint apiTokenInfo (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.bookings.multipleBookingSync) req
