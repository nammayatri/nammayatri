{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.RideBooking.Select where

import qualified "rider-app" API.Dashboard.RideBooking.Select as BAP
import qualified Dashboard.RiderPlatform.Customer as Common
import qualified "rider-app" Domain.Action.UI.Select as DSelect
import qualified "rider-app" Domain.Types.Estimate as DEstimate
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "rider-app" Domain.Types.Person as DP
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp.RideBooking as Client
import Servant
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "select"
    :> ( ApiAuth 'APP_BACKEND 'CUSTOMERS 'SELECT
           :> BAP.CustomerSelectAPI
           :<|> ApiAuth 'APP_BACKEND 'CUSTOMERS 'SELECTLIST
             :> BAP.CustomerSelectListAPI
           :<|> ApiAuth 'APP_BACKEND 'CUSTOMERS 'SELECTRESULT
             :> BAP.CustomerSelectResultAPI
           :<|> ApiAuth 'APP_BACKEND 'CUSTOMERS 'CANCELSEARCH
             :> BAP.CustomerCancelSearchAPI
       )

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  callSelect merchantId city
    :<|> callSelectList merchantId city
    :<|> callSelectResult merchantId city
    :<|> callCancelSearch merchantId city

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  BAP.RideEstimatesEndPoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.SelectAPI endpoint) (Just APP_BACKEND) (Just apiTokenInfo) Nothing Nothing

callSelect :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id DP.Person -> Id DEstimate.Estimate -> FlowHandler APISuccess
callSelect merchantShortId opCity apiTokenInfo personId estimateId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction BAP.EstimatesEndPoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId opCity (.rideBooking.select.rSelect) personId estimateId

callSelectList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id DP.Person -> Id DEstimate.Estimate -> FlowHandler DSelect.SelectListRes
callSelectList merchantShortId opCity apiTokenInfo personId estimateId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderApp checkedMerchantId opCity (.rideBooking.select.selectList) personId estimateId

callSelectResult :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id DP.Person -> Id DEstimate.Estimate -> FlowHandler DSelect.QuotesResultResponse
callSelectResult merchantShortId opCity apiTokenInfo personId estimateId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderApp checkedMerchantId opCity (.rideBooking.select.selectResult) personId estimateId

callCancelSearch :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id DP.Person -> Id DEstimate.Estimate -> FlowHandler DSelect.CancelAPIResponse
callCancelSearch merchantShortId opCity apiTokenInfo personId estimateId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction BAP.CancelSearchEndPoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId opCity (.rideBooking.select.cancelSearch) personId estimateId
