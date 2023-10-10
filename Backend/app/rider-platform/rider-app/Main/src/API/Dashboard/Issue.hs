module API.Dashboard.Issue where

import qualified Domain.Types.Merchant as DM
import Environment
import qualified IssueManagement.API.Dashboard.Issue as IMD
import qualified IssueManagement.Common as Common
import qualified IssueManagement.Common.Dashboard.Issue as Common
import qualified IssueManagement.Domain.Action.Dashboard.Issue as DIssue
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueReport
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (Unauthorized, throwError)
import Storage.Beam.IssueManagement ()
import qualified Storage.Queries.Person as QP

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

dashboardIssueHandle :: DIssue.ServiceHandle Flow
dashboardIssueHandle =
  DIssue.ServiceHandle
    { findPersonById = castPersonById
    }

castPersonById :: EsqDBReplicaFlow m r => Id Common.Person -> m (Maybe Common.Person)
castPersonById driverId = do
  person <- runInReplica $ QP.findById (cast driverId)
  return $ fmap castPerson person
  where
    castPerson person =
      Common.Person
        { id = cast person.id,
          language = person.language,
          firstName = person.firstName,
          lastName = person.lastName,
          middleName = person.middleName,
          mobileNumber = person.mobileNumber
        }

issueCategoryList ::
  ShortId DM.Merchant ->
  FlowHandler Common.IssueCategoryListRes
issueCategoryList (ShortId merchantShortId) = withFlowHandlerAPI $ DIssue.issueCategoryList (ShortId merchantShortId) Common.CUSTOMER

issueList ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.IssueStatus ->
  Maybe (Id IssueCategory) ->
  Maybe Text ->
  FlowHandler Common.IssueReportListResponse
issueList (ShortId merchantShortId) mbLimit mbOffset mbStatus mbCategoryId mbAssignee = withFlowHandlerAPI $ DIssue.issueList (ShortId merchantShortId) mbLimit mbOffset mbStatus (cast <$> mbCategoryId) mbAssignee Common.CUSTOMER

issueInfo ::
  ShortId DM.Merchant ->
  Id IssueReport ->
  FlowHandler Common.IssueInfoRes
issueInfo (ShortId merchantShortId) issueReportId = withFlowHandlerAPI $ DIssue.issueInfo (ShortId merchantShortId) (cast issueReportId) dashboardIssueHandle Common.CUSTOMER

issueUpdate ::
  ShortId DM.Merchant ->
  Id IssueReport ->
  Common.IssueUpdateByUserReq ->
  FlowHandler APISuccess
issueUpdate (ShortId merchantShortId) issueReportId req = withFlowHandlerAPI $ DIssue.issueUpdate (ShortId merchantShortId) (cast issueReportId) req

issueAddComment ::
  ShortId DM.Merchant ->
  Id IssueReport ->
  Common.IssueAddCommentByUserReq ->
  FlowHandler APISuccess
issueAddComment (ShortId merchantShortId) issueReportId req = withFlowHandlerAPI $ DIssue.issueAddComment (ShortId merchantShortId) (cast issueReportId) req

issueFetchMedia :: ShortId DM.Merchant -> Text -> FlowHandler Text
issueFetchMedia (ShortId merchantShortId) = withFlowHandlerAPI . DIssue.issueFetchMedia (ShortId merchantShortId)

ticketStatusCallBack :: ShortId DM.Merchant -> Common.TicketStatusCallBackReq -> FlowHandler APISuccess
ticketStatusCallBack (ShortId merchantShortId) = withFlowHandlerAPI . DIssue.ticketStatusCallBack (ShortId merchantShortId) Common.CUSTOMER
