module API.Dashboard.Issue where

import qualified Domain.Action.Dashboard.Issue as DIssue
import qualified Domain.Types.Merchant as DM
import Environment
import qualified IssueManagement.API.Dashboard.Issue as IMD
import qualified IssueManagement.Common as Common
import qualified IssueManagement.Common.Dashboard.Issue as Common
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueReport
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (Unauthorized, throwError)

type API = IMD.DashboardIssueAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  issueCategoryList merchantId
    :<|> issueList merchantId
    :<|> issueInfo merchantId
    :<|> issueUpdate merchantId
    :<|> issueAddComment merchantId
    :<|> issueFetchMedia merchantId
    :<|> ticketStatusCallBack merchantId

issueCategoryList ::
  ShortId DM.Merchant ->
  FlowHandler Common.IssueCategoryListRes
issueCategoryList merchantShortId = withFlowHandlerAPI $ DIssue.issueCategoryList merchantShortId Common.CUSTOMER

issueList ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.IssueStatus ->
  Maybe (Id IssueCategory) ->
  Maybe Text ->
  FlowHandler Common.IssueReportListResponse
issueList merchantShortId mbLimit mbOffset mbStatus mbCategoryId mbAssignee = withFlowHandlerAPI $ DIssue.issueList merchantShortId mbLimit mbOffset mbStatus (cast <$> mbCategoryId) mbAssignee Common.CUSTOMER

issueInfo ::
  ShortId DM.Merchant ->
  Id IssueReport ->
  FlowHandler Common.IssueInfoRes
issueInfo merchantShortId issueReportId = withFlowHandlerAPI $ DIssue.issueInfo merchantShortId (cast issueReportId) Common.CUSTOMER

issueUpdate ::
  ShortId DM.Merchant ->
  Id IssueReport ->
  Common.IssueUpdateByUserReq ->
  FlowHandler APISuccess
issueUpdate merchantShortId issueReportId req = withFlowHandlerAPI $ DIssue.issueUpdate merchantShortId (cast issueReportId) req

issueAddComment ::
  ShortId DM.Merchant ->
  Id IssueReport ->
  Common.IssueAddCommentByUserReq ->
  FlowHandler APISuccess
issueAddComment merchantShortId issueReportId req = withFlowHandlerAPI $ DIssue.issueAddComment merchantShortId (cast issueReportId) req

issueFetchMedia :: ShortId DM.Merchant -> Text -> FlowHandler Text
issueFetchMedia merchantShortId = withFlowHandlerAPI . DIssue.issueFetchMedia merchantShortId

ticketStatusCallBack :: ShortId DM.Merchant -> Common.TicketStatusCallBackReq -> FlowHandler APISuccess
ticketStatusCallBack merchantShortId = withFlowHandlerAPI . DIssue.ticketStatusCallBack merchantShortId Common.CUSTOMER
