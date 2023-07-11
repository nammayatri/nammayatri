{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Volunteer
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Volunteer as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI)
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth hiding (BECKN_TRANSPORT)
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "volunteer"
    :> ( BookingInfoAPI
           :<|> AssignCreateAndStartOtpRideAPI
       )

type BookingInfoAPI =
  ApiAuth ('DriverOfferBPP ('VOLUNTEER 'BOOKING_INFO))
    :> Common.BookingInfoAPI

type AssignCreateAndStartOtpRideAPI =
  ApiAuth ('DriverOfferBPP ('VOLUNTEER 'ASSIGN_CREATE_AND_START_OTP_RIDE))
    :> Common.AssignCreateAndStartOtpRideAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  bookingInfo merchantId
    :<|> assignCreateAndStartOtpRide merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.VolunteerEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.VolunteerAPI endpoint) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) Nothing Nothing

bookingInfo :: ShortId DM.Merchant -> ApiTokenInfo -> Text -> FlowHandler Common.BookingInfoResponse
bookingInfo merchantShortId apiTokenInfo otpCode = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.volunteer.bookingInfo) otpCode

assignCreateAndStartOtpRide :: ShortId DM.Merchant -> ApiTokenInfo -> Common.AssignCreateAndStartOtpRideAPIReq -> FlowHandler APISuccess
assignCreateAndStartOtpRide merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.AssignCreateAndStartOtpRideEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.volunteer.assignCreateAndStartOtpRide) req
