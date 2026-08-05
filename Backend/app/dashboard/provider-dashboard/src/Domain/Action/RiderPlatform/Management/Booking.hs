{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.RiderPlatform.Management.Booking
  ( postBookingCancelAllStuck,
    postBookingSyncMultiple,
  )
where

import qualified API.Client.RiderPlatform.Management as Client
import qualified "dashboard-helper-api" API.Types.RiderPlatform.Management.Booking as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow)
import Kernel.Utils.Validation (runRequestValidation)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth hiding (DRIVER_OFFER_BPP)
import "lib-dashboard" Tools.Auth.Merchant

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction apiTokenInfo =
  T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just APP_BACKEND_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

postBookingCancelAllStuck :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.StuckBookingsCancelReq -> Flow Common.StuckBookingsCancelRes
postBookingCancelAllStuck merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.bookingDSL.postBookingCancelAllStuck) req

postBookingSyncMultiple :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.MultipleBookingSyncReq -> Flow Common.MultipleBookingSyncResp
postBookingSyncMultiple merchantShortId opCity apiTokenInfo req = do
  runRequestValidation Common.validateMultipleBookingSyncReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.bookingDSL.postBookingSyncMultiple) req
