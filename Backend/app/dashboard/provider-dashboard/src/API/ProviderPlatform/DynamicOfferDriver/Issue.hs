{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Issue
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Issue as Common
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
  "issue"
    :> ( IssueListAPI
           :<|> IssueUpdateAPI
           :<|> IssueAddCommentAPI
       )

type IssueListAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'ISSUE
    :> Common.IssueListAPI

type IssueUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'ISSUE
    :> Common.IssueUpdateAPI

type IssueAddCommentAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'ISSUE
    :> Common.IssueAddCommentAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  issueList merchantId
    :<|> issueUpdate merchantId
    :<|> issueAddComment merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.IssueEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo = T.buildTransaction (DT.IssueAPI endpoint) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) Nothing Nothing

issueList :: ShortId DM.Merchant -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Common.IssueStatus -> Maybe Text -> Maybe Text -> FlowHandler Common.IssueReportListResponse
issueList merchantShortId apiTokenInfo mbLimit mbOffset mbStatus mbCategory mbAssignee = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.issue.issueList) mbLimit mbOffset mbStatus mbCategory mbAssignee

issueUpdate :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.IssueReport -> Common.IssueUpdateReq -> FlowHandler APISuccess
issueUpdate merchantShortId apiTokenInfo issueReportId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.IssueUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.issue.issueUpdate) issueReportId req

issueAddComment :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.IssueReport -> Common.IssueAddCommentReq -> FlowHandler APISuccess
issueAddComment merchantShortId apiTokenInfo issueReportId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.IssueAddCommentEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.issue.issueAddComment) issueReportId req