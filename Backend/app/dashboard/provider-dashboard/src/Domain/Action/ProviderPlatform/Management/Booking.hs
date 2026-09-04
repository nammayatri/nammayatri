{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.ProviderPlatform.Management.Booking
  ( postBookingCancelAllStuck,
    postBookingCancelAllStuckInternal,
    postBookingSyncMultiple,
    postBookingSyncMultipleInternal,
  )
where

import qualified API.Client.ProviderPlatform.Management as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management as Management
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Booking as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (encodeToText)
import Kernel.Utils.Validation (runRequestValidation)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

-- | Transaction builder shared by the RBAC-guarded dashboard route and the
-- internal service-token route for the same endpoint. Records an audit row in
-- the dashboard transaction table; @requestor@ is @Just personId.getId@ for
-- person-token calls and a caller tag (e.g. "INTERNAL_ADMIN_API") for
-- service-token calls, so internal actions stay attributable.
buildBookingTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  DT.Endpoint ->
  Maybe Text ->
  Maybe (Id DM.Merchant) ->
  Maybe request ->
  m DT.Transaction
buildBookingTransaction endpoint mbRequestor mbMerchantId request = do
  T.validateRequestorId mbRequestor
  uid <- generateGUID
  now <- getCurrentTime
  pure
    DT.Transaction
      { id = uid,
        requestorId = Id <$> mbRequestor,
        serverName = Just DRIVER_OFFER_BPP_MANAGEMENT,
        merchantId = mbMerchantId,
        endpoint,
        commonDriverId = Nothing,
        commonRideId = Nothing,
        request = encodeToText . Common.hideSecrets <$> request,
        response = Nothing,
        responseError = Nothing,
        createdAt = now
      }

postBookingCancelAllStuck :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.StuckBookingsCancelReq -> Flow Common.StuckBookingsCancelRes
postBookingCancelAllStuck merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  postBookingCancelAllStuckInternal checkedMerchantId opCity (Just apiTokenInfo.personId.getId) (Just apiTokenInfo.merchant.id) req

-- | Implementation shared by the public RBAC route and the internal
-- service-token route (@API.ProviderPlatform.DynamicOfferDriver.InternalAdmin@).
-- Callers must have resolved and validated the merchant/city themselves: the
-- merchant-city-vs-token check is the public route's job, the @api-key@ check
-- the internal route's job.
postBookingCancelAllStuckInternal :: CheckedShortId DM.Merchant -> City.City -> Maybe Text -> Maybe (Id DM.Merchant) -> Common.StuckBookingsCancelReq -> Flow Common.StuckBookingsCancelRes
postBookingCancelAllStuckInternal checkedMerchantId opCity mbRequestor mbMerchantId req = do
  transaction <- buildBookingTransaction (DT.castEndpoint $ PROVIDER_MANAGEMENT $ Management.BOOKING Common.POST_BOOKING_CANCEL_ALL_STUCK) mbRequestor mbMerchantId (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.bookingDSL.postBookingCancelAllStuck) req

postBookingSyncMultiple :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.MultipleBookingSyncReq -> Flow Common.MultipleBookingSyncResp
postBookingSyncMultiple merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  postBookingSyncMultipleInternal checkedMerchantId opCity (Just apiTokenInfo.personId.getId) (Just apiTokenInfo.merchant.id) req

postBookingSyncMultipleInternal :: CheckedShortId DM.Merchant -> City.City -> Maybe Text -> Maybe (Id DM.Merchant) -> Common.MultipleBookingSyncReq -> Flow Common.MultipleBookingSyncResp
postBookingSyncMultipleInternal checkedMerchantId opCity mbRequestor mbMerchantId req = do
  runRequestValidation Common.validateMultipleBookingSyncReq req
  transaction <- buildBookingTransaction (DT.castEndpoint $ PROVIDER_MANAGEMENT $ Management.BOOKING Common.POST_BOOKING_SYNC_MULTIPLE) mbRequestor mbMerchantId (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.bookingDSL.postBookingSyncMultiple) req
