module API.UI.Issue where

import qualified Domain.Action.UI.Issue as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import IssueManagement.API.UI.Issue as IA
import IssueManagement.Common.UI.Issue
import qualified IssueManagement.Common.UI.Issue as Common
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as Domain
import qualified IssueManagement.Domain.Types.Issue.IssueOption as Domain
import qualified IssueManagement.Domain.Types.Issue.IssueReport as Domain
import Kernel.External.Types (Language)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "issue" :> TokenAuth :> IA.IssueAPI

handler :: FlowServer API
handler = externalHandler
  where
    externalHandler (personId, merchantId) =
      createIssueReport (personId, merchantId)
        :<|> issueReportCustomerList (personId, merchantId)
        :<|> issueMediaUpload (personId, merchantId)
        :<|> fetchMedia (personId, merchantId)
        :<|> getIssueCategory (personId, merchantId)
        :<|> getIssueOption (personId, merchantId)
        :<|> issueInfo (personId, merchantId)
        :<|> updateIssueOption (personId, merchantId)
        :<|> deleteIssue (personId, merchantId)
        :<|> updateIssueStatus (personId, merchantId)

issueReportCustomerList :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> FlowHandler Common.IssueReportListRes
issueReportCustomerList (personId, merchantId) language = withFlowHandlerAPI $ Common.issueReportList (cast personId, cast merchantId) language CUSTOMER

fetchMedia :: (Id SP.Person, Id DM.Merchant) -> Text -> FlowHandler Text
fetchMedia (personId, merchantId) = withFlowHandlerAPI . Common.fetchMedia (cast personId, cast merchantId)

createIssueReport :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> Common.IssueReportReq -> FlowHandler Common.IssueReportRes
createIssueReport (personId, merchantId) mbLanguage req = withFlowHandlerAPI $ Common.createIssueReport (cast personId, cast merchantId) mbLanguage req CUSTOMER

issueMediaUpload :: (Id SP.Person, Id DM.Merchant) -> Common.IssueMediaUploadReq -> FlowHandler Common.IssueMediaUploadRes
issueMediaUpload (personId, merchantId) req = withFlowHandlerAPI $ Common.issueMediaUpload (cast personId, cast merchantId) req

issueInfo :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Maybe Language -> FlowHandler Common.IssueInfoRes
issueInfo (personId, merchantId) issueReportId language = withFlowHandlerAPI $ Common.issueInfo issueReportId (cast personId, cast merchantId) language CUSTOMER

updateIssueOption :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Common.IssueUpdateReq -> FlowHandler APISuccess
updateIssueOption (personId, merchantId) issueReportId req = withFlowHandlerAPI $ Common.updateIssueOption issueReportId (cast personId, cast merchantId) req CUSTOMER

deleteIssue :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> FlowHandler APISuccess
deleteIssue (personId, merchantId) issueReportId = withFlowHandlerAPI $ Common.deleteIssue issueReportId (cast personId, cast merchantId) CUSTOMER

getIssueCategory :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> FlowHandler Common.IssueCategoryListRes
getIssueCategory (personId, merchantId) language = withFlowHandlerAPI $ Common.getIssueCategory (cast personId, cast merchantId) language CUSTOMER

getIssueOption :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueCategory -> Maybe (Id Domain.IssueOption) -> Maybe (Id Domain.IssueReport) -> Maybe Language -> FlowHandler Common.IssueOptionListRes
getIssueOption (personId, merchantId) issueCategoryId issueOptionId issueReportId language = withFlowHandlerAPI $ Common.getIssueOption (cast personId, cast merchantId) issueCategoryId issueOptionId issueReportId language CUSTOMER

updateIssueStatus :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Maybe Language -> Common.IssueStatusUpdateReq -> FlowHandler Common.IssueStatusUpdateRes
updateIssueStatus (personId, merchantId) issueReportId language req = withFlowHandlerAPI $ Common.updateIssueStatus (cast personId, cast merchantId) issueReportId language req CUSTOMER
