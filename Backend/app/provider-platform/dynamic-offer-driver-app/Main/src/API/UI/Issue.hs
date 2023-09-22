module API.UI.Issue where

import qualified Domain.Action.UI.Issue as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.API.UI.Issue as IA
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as Domain
import qualified IssueManagement.Domain.Types.Issue.IssueOption as Domain
import qualified IssueManagement.Domain.Types.Issue.IssueReport as Domain
import qualified IssueManagement.ProviderPlatform.Issue as Common
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
        :<|> issueReportDriverList (personId, merchantId)
        :<|> issueMediaUpload (personId, merchantId)
        :<|> fetchMedia (personId, merchantId)
        :<|> getIssueCategory (personId, merchantId)
        :<|> getIssueOption (personId, merchantId)
        :<|> issueInfo (personId, merchantId)
        :<|> updateIssueOption (personId, merchantId)
        :<|> deleteIssue (personId, merchantId)
        :<|> updateIssueStatus (personId, merchantId)

issueReportDriverList :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> FlowHandler Common.IssueReportListRes
issueReportDriverList (driverId, merchantId) language = withFlowHandlerAPI $ Common.issueReportList (cast driverId, cast merchantId) language Common.DRIVER

fetchMedia :: (Id SP.Person, Id DM.Merchant) -> Text -> FlowHandler Text
fetchMedia (driverId, merchantId) filePath = withFlowHandlerAPI $ Common.fetchMedia (cast driverId, cast merchantId) filePath

createIssueReport :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> Common.IssueReportReq -> FlowHandler Common.IssueReportRes
createIssueReport (driverId, merchantId) mbLanguage req = withFlowHandlerAPI $ Common.createIssueReport (cast driverId, cast merchantId) mbLanguage req Common.DRIVER

issueMediaUpload :: (Id SP.Person, Id DM.Merchant) -> Common.IssueMediaUploadReq -> FlowHandler Common.IssueMediaUploadRes
issueMediaUpload (driverId, merchantId) req = withFlowHandlerAPI $ Common.issueMediaUpload (cast driverId, cast merchantId) req

issueInfo :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Maybe Language -> FlowHandler Common.IssueInfoRes
issueInfo (driverId, merchantId) issueReportId language = withFlowHandlerAPI $ Common.issueInfo issueReportId (cast driverId, cast merchantId) language Common.DRIVER

updateIssueOption :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Common.IssueUpdateReq -> FlowHandler APISuccess
updateIssueOption (driverId, merchantId) issueReportId req = withFlowHandlerAPI $ Common.updateIssueOption issueReportId (cast driverId, cast merchantId) req Common.DRIVER

deleteIssue :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> FlowHandler APISuccess
deleteIssue (driverId, merchantId) issueReportId = withFlowHandlerAPI $ Common.deleteIssue issueReportId (cast driverId, cast merchantId) Common.DRIVER

getIssueCategory :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> FlowHandler Common.IssueCategoryListRes
getIssueCategory (driverId, merchantId) language = withFlowHandlerAPI $ Common.getIssueCategory (cast driverId, cast merchantId) language Common.DRIVER

getIssueOption :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueCategory -> Maybe (Id Domain.IssueOption) -> Maybe (Id Domain.IssueReport) -> Maybe Language -> FlowHandler Common.IssueOptionListRes
getIssueOption (driverId, merchantId) issueCategoryId issueOptionId issueReportId language = withFlowHandlerAPI $ Common.getIssueOption (cast driverId, cast merchantId) issueCategoryId issueOptionId issueReportId language Common.DRIVER

updateIssueStatus :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Maybe Language -> Common.IssueStatusUpdateReq -> FlowHandler Common.IssueStatusUpdateRes
updateIssueStatus (driverId, merchantId) issueReportId language req = withFlowHandlerAPI $ Common.updateIssueStatus (cast driverId, cast merchantId) issueReportId language req Common.DRIVER
