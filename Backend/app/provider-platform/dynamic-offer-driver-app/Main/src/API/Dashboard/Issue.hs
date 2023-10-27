module API.Dashboard.Issue where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Issue as Common
import qualified Domain.Action.Dashboard.Issue as DIssue
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
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
           :<|> Common.IssueFetchMediaAPI
           :<|> Common.TicketStatusCallBackAPI
       )

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  issueCategoryList merchantId city
    :<|> issueList merchantId city
    :<|> issueInfo merchantId city
    :<|> issueUpdate merchantId city
    :<|> issueAddComment merchantId city
    :<|> issueFetchMedia merchantId city
    :<|> ticketStatusCallBack merchantId city

issueCategoryList ::
  ShortId DM.Merchant ->
  City.City ->
  FlowHandler Common.IssueCategoryListRes
issueCategoryList merchantShortId = withFlowHandlerAPI . DIssue.issueCategoryList merchantShortId

issueList ::
  ShortId DM.Merchant ->
  City.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.IssueStatus ->
  Maybe (Id Common.IssueCategory) ->
  Maybe Text ->
  FlowHandler Common.IssueReportListResponse
issueList merchantShortId opCity mbLimit mbOffset mbStatus mbCategoryId = withFlowHandlerAPI . DIssue.issueList merchantShortId opCity mbLimit mbOffset mbStatus (cast <$> mbCategoryId)

issueInfo ::
  ShortId DM.Merchant ->
  City.City ->
  Id Common.IssueReport ->
  FlowHandler Common.IssueInfoRes
issueInfo merchantShortId opCity issueReportId = withFlowHandlerAPI $ DIssue.issueInfo merchantShortId opCity (cast issueReportId)

issueUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  Id Common.IssueReport ->
  Common.IssueUpdateByUserReq ->
  FlowHandler APISuccess
issueUpdate merchantShortId opCity issueReportId = withFlowHandlerAPI . DIssue.issueUpdate merchantShortId opCity (cast issueReportId)

issueAddComment ::
  ShortId DM.Merchant ->
  City.City ->
  Id Common.IssueReport ->
  Common.IssueAddCommentByUserReq ->
  FlowHandler APISuccess
issueAddComment merchantShortId opCity issueReportId = withFlowHandlerAPI . DIssue.issueAddComment merchantShortId opCity (cast issueReportId)

issueFetchMedia :: ShortId DM.Merchant -> City.City -> Text -> FlowHandler Text
issueFetchMedia merchantShortId opCity = withFlowHandlerAPI . DIssue.issueFetchMedia merchantShortId opCity

ticketStatusCallBack :: ShortId DM.Merchant -> City.City -> Common.TicketStatusCallBackReq -> FlowHandler APISuccess
ticketStatusCallBack merchantShortId opCity = withFlowHandlerAPI . DIssue.ticketStatusCallBack merchantShortId opCity
