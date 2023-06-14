{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.CallbackRequest where

import qualified "rider-app" API.Dashboard.CallbackRequest as BAP
import qualified Dashboard.RiderPlatform.Customer as Common
import qualified "rider-app" Domain.Action.Dashboard.CallbackRequest as DC
import qualified "rider-app" Domain.Types.CallbackRequest as DCR
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp as Client
import Servant
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "callback"
    :> ( ApiAuth 'APP_BACKEND 'CUSTOMERS 'LIST_CALLBACK_REQUEST
           :> BAP.ListCallbackRequest
           :<|> ApiAuth 'APP_BACKEND 'CUSTOMERS 'UPDATE_CALLBACK_STATUS
             :> BAP.UpdateCallbackRequestSatus
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listCallbackRequest merchantId
    :<|> updateCallbackRequest merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  BAP.CallbackRequestEndPoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.CallbackRequest endpoint) (Just APP_BACKEND) (Just apiTokenInfo) Nothing Nothing

listCallbackRequest :: ShortId DM.Merchant -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe DCR.CallbackRequestStatus -> FlowHandler DC.CallbackRequestRes
listCallbackRequest merchantShortId apiTokenInfo mbLimit mbOffset mbMobileNumber mbCreatedAt mbStatus = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callRiderApp checkedMerchantId (.callback.listCallbackRequest) mbLimit mbOffset mbMobileNumber mbCreatedAt mbStatus

updateCallbackRequest :: ShortId DM.Merchant -> ApiTokenInfo -> Id DCR.CallbackRequest -> DCR.CallbackRequestStatus -> FlowHandler APISuccess
updateCallbackRequest merchantShortId apiTokenInfo id status = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction BAP.CallbackRequestEndPoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.callback.updateCallbackRequest) id status
