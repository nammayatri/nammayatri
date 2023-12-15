{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.Issue
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.Common as DC
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import qualified IssueManagement.Common as DIssue
import qualified IssueManagement.Common.Dashboard.Issue as Common
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueReport
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error (PersonError (..))
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI')
import Kernel.Utils.Error (fromMaybeM)
import qualified RiderPlatformClient.RiderApp.Operations as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import qualified "lib-dashboard" Storage.Queries.Person as QP
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "issueV2"
    :> ( IssueCategoryListAPI
           :<|> IssueListAPI
           :<|> IssueInfoAPI
           :<|> IssueUpdateAPI
           :<|> IssueAddCommentAPI
           :<|> IssueFetchMediaAPI
           :<|> TicketStatusCallBackAPI
       )

type IssueCategoryListAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'ISSUE 'ISSUE_CATEGORY_LIST
    :> Common.IssueCategoryListAPI

type IssueListAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'ISSUE 'ISSUE_LIST
    :> Common.IssueListAPI

type IssueInfoAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'ISSUE 'ISSUE_INFO
    :> Common.IssueInfoAPI

type IssueUpdateAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'ISSUE 'ISSUE_UPDATE
    :> Common.IssueUpdateAPI

type IssueAddCommentAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'ISSUE 'ISSUE_ADD_COMMENT
    :> Common.IssueAddCommentAPI

type IssueFetchMediaAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'ISSUE 'ISSUE_FETCH_MEDIA
    :> Common.IssueFetchMediaAPI

type TicketStatusCallBackAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'ISSUE 'TICKET_STATUS_CALL_BACK
    :> Common.TicketStatusCallBackAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  issueCategoryList merchantId city
    :<|> issueList merchantId city
    :<|> issueInfo merchantId city
    :<|> issueUpdate merchantId city
    :<|> issueAddComment merchantId city
    :<|> issueFetchMedia merchantId city
    :<|> ticketStatusCallBack merchantId city

buildTransaction ::
  ( MonadFlow m,
    DC.HideSecrets request
  ) =>
  Common.IssueEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo = T.buildTransaction (DT.IssueAPI endpoint) (Just APP_BACKEND_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

issueCategoryList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> FlowHandler Common.IssueCategoryListRes
issueCategoryList merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderAppOperations checkedMerchantId opCity (.issuesV2.issueCategoryList)

issueList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe DIssue.IssueStatus -> Maybe (Id IssueCategory) -> Maybe Text -> FlowHandler Common.IssueReportListResponse
issueList merchantShortId opCity apiTokenInfo mbLimit mbOffset mbStatus mbCategoryId mbAssignee = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderAppOperations checkedMerchantId opCity (.issuesV2.issueList) mbLimit mbOffset mbStatus mbCategoryId mbAssignee

issueInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id IssueReport -> FlowHandler Common.IssueInfoRes
issueInfo merchantShortId opCity apiTokenInfo issueReportId_ = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  addAuthorDetails =<< Client.callRiderAppOperations checkedMerchantId opCity (.issuesV2.issueInfo) issueReportId_
  where
    mkAuthorDetail :: Common.IssueReportCommentItem -> Flow Common.IssueReportCommentItem
    mkAuthorDetail Common.IssueReportCommentItem {..} = do
      author <- Esq.runInReplica (QP.findById $ cast authorDetail.authorId) >>= fromMaybeM (PersonNotFound authorDetail.authorId.getId)
      let authorDetail_ =
            Common.AuthorDetail
              { authorId = cast author.id,
                firstName = Just author.firstName,
                lastName = Just author.lastName
              }
      pure $
        Common.IssueReportCommentItem
          { authorDetail = authorDetail_,
            ..
          }
    addAuthorDetails :: Common.IssueInfoRes -> Flow Common.IssueInfoRes
    addAuthorDetails Common.IssueInfoRes {..} = do
      comments_ <- mapM mkAuthorDetail comments
      pure $
        Common.IssueInfoRes
          { comments = comments_,
            ..
          }

issueUpdate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id IssueReport -> Common.IssueUpdateReq -> FlowHandler APISuccess
issueUpdate merchantShortId opCity apiTokenInfo issueReportId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.IssueUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.issuesV2.issueUpdate) issueReportId (mkRequest req)
  where
    mkRequest Common.IssueUpdateReq {..} =
      Common.IssueUpdateByUserReq
        { userId = cast apiTokenInfo.personId,
          ..
        }

issueAddComment :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id IssueReport -> Common.IssueAddCommentReq -> FlowHandler APISuccess
issueAddComment merchantShortId opCity apiTokenInfo issueReportId req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.IssueAddCommentEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.issuesV2.issueAddComment) issueReportId (mkRequest req)
  where
    mkRequest Common.IssueAddCommentReq {..} =
      Common.IssueAddCommentByUserReq
        { userId = cast apiTokenInfo.personId,
          ..
        }

issueFetchMedia :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> FlowHandler Text
issueFetchMedia merchantShortId opCity apiTokenInfo filePath = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderAppOperations checkedMerchantId opCity (.issuesV2.issueFetchMedia) filePath

ticketStatusCallBack :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.TicketStatusCallBackReq -> FlowHandler APISuccess
ticketStatusCallBack merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.TicketStatusCallBackEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ Client.callRiderAppOperations checkedMerchantId opCity (.issuesV2.ticketStatusCallBack_) req
