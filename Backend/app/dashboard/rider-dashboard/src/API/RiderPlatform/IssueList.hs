{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.IssueList where

import qualified "rider-app" API.Dashboard.IssueList as BAP
import qualified "rider-app" Domain.Action.Dashboard.IssueList as DI
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import IssueManagement.Common.Dashboard.Issue as Common hiding (IssueListAPI, TicketStatusCallBackAPI)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp as Client
import Servant
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "issue"
    :> ( IssueListAPI
           :<|> TicketStatusCallBackAPI
       )

type IssueListAPI =
  ApiAuth 'APP_BACKEND 'CUSTOMERS 'LIST_ISSUE
    :> BAP.ListCustomerIssue

type TicketStatusCallBackAPI =
  ApiAuth 'APP_BACKEND 'ISSUE 'TICKET_STATUS_CALL_BACK
    :> BAP.TicketStatusCallBack

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listIssue merchantId
    :<|> ticketStatusCallBack merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.IssueEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo = T.buildTransaction (DT.IssueAPI endpoint) (Just APP_BACKEND) (Just apiTokenInfo) Nothing Nothing

listIssue :: ShortId DM.Merchant -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> FlowHandler DI.IssueListRes
listIssue merchantShortId apiTokenInfo mbLimit mbOffset mbMobileCountryCode mbMobileNumber mbFrom mbTo = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callRiderApp checkedMerchantId (.issues.listIssue) mbLimit mbOffset mbMobileCountryCode mbMobileNumber mbFrom mbTo

ticketStatusCallBack :: ShortId DM.Merchant -> ApiTokenInfo -> Common.TicketStatusCallBackReq -> FlowHandler APISuccess
ticketStatusCallBack merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.TicketStatusCallBackEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callRiderApp checkedMerchantId (.issues.ticketStatusCallBack) req
