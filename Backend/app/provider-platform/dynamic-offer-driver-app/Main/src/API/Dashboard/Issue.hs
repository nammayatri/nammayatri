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
    :> ( Common.IssueListAPI
          :<|> Common.IssueUpdateAPI
          :<|> Common.IssueAddCommentAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  issueList merchantId
    :<|> issueUpdate merchantId
    :<|> issueAddComment merchantId

issueList ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.IssueStatus ->
  Maybe Text ->
  Maybe Text ->
  FlowHandler Common.IssueReportListResponse
issueList merchantShortId mbLimit mbOffset mbStatus mbCategory = withFlowHandlerAPI . DIssue.issueList merchantShortId mbLimit mbOffset mbStatus mbCategory

issueUpdate ::
  ShortId DM.Merchant ->
  Id Common.IssueReport ->
  Common.IssueUpdateReq ->
  FlowHandler APISuccess
issueUpdate merchantShortId issueReportId = withFlowHandlerAPI . DIssue.issueUpdate merchantShortId (cast issueReportId)

issueAddComment ::
  ShortId DM.Merchant ->
  Id Common.IssueReport ->
  Common.IssueAddCommentReq ->
  FlowHandler APISuccess
issueAddComment merchantShortId issueReportId = withFlowHandlerAPI . DIssue.issueAddComment merchantShortId (cast issueReportId)