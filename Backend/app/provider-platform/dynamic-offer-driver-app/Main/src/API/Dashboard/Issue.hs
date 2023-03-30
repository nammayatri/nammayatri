module API.Dashboard.Issue where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Issue as Common
import qualified Domain.Action.Dashboard.Issue as DIssue
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (Unauthorized, throwError)

type API =
  "issue"
    :> ( Common.IssueCategoryListAPI
           :<|> Common.IssueListAPI
           :<|> Common.IssueInfoAPI
           :<|> Common.IssueUpdateByUserAPI
           :<|> Common.IssueAddCommentByUserAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  issueCategoryList merchantId
    :<|> issueList merchantId
    :<|> issueInfo merchantId
    :<|> issueUpdate merchantId
    :<|> issueAddComment merchantId

issueCategoryList ::
  ShortId DM.Merchant ->
  FlowHandler Common.IssueCategoryListRes
issueCategoryList = withFlowHandlerAPI . DIssue.issueCategoryList

issueList ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.IssueStatus ->
  Maybe (Id Common.IssueCategory) ->
  Maybe Text ->
  FlowHandler Common.IssueReportListResponse
issueList merchantShortId mbLimit mbOffset mbStatus mbCategoryId = withFlowHandlerAPI . DIssue.issueList merchantShortId mbLimit mbOffset mbStatus (cast <$> mbCategoryId)

issueInfo ::
  ShortId DM.Merchant ->
  Id Common.IssueReport ->
  FlowHandler Common.IssueInfoRes
issueInfo merchantShortId issueReportId = withFlowHandlerAPI $ DIssue.issueInfo merchantShortId (cast issueReportId)

issueUpdate ::
  ShortId DM.Merchant ->
  Id Common.IssueReport ->
  Common.IssueUpdateByUserReq ->
  FlowHandler APISuccess
issueUpdate merchantShortId issueReportId = withFlowHandlerAPI . DIssue.issueUpdate merchantShortId (cast issueReportId)

issueAddComment ::
  ShortId DM.Merchant ->
  Id Common.IssueReport ->
  Common.IssueAddCommentByUserReq ->
  FlowHandler APISuccess
issueAddComment merchantShortId issueReportId = withFlowHandlerAPI . DIssue.issueAddComment merchantShortId (cast issueReportId)
