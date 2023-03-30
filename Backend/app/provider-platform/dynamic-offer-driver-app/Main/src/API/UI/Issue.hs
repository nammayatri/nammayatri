module API.UI.Issue where

import qualified Domain.Action.UI.Issue as Domain
import qualified Domain.Types.Issue.IssueCategory as Domain
import qualified Domain.Types.Issue.IssueReport as Domain
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Types (Language)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified "shared-services" SharedService.ProviderPlatform.Issue as Common
import Tools.Auth

type API =
  "issue"
    :> ( TokenAuth
           :> Common.IssueCreateAPI
           :<|> "list"
             :> TokenAuth
             :> Common.IssueListAPI
           :<|> "upload"
             :> TokenAuth
             :> Common.IssueUploadAPI
           :<|> "media"
             :> TokenAuth
             :> Common.IssueFetchMediaAPI
           :<|> "category"
             :> TokenAuth
             :> Common.IssueCategoryAPI
           :<|> "option"
             :> TokenAuth
             :> Common.IssueOptionAPI
           :<|> Capture "issueId" (Id Domain.IssueReport)
             :> "info"
             :> TokenAuth
             :> Common.IssueInfoAPI
           :<|> Capture "issueId" (Id Domain.IssueReport)
             :> "option"
             :> TokenAuth
             :> Common.IssueUpdateAPI
           :<|> Capture "issueId" (Id Domain.IssueReport)
             :> "delete"
             :> TokenAuth
             :> Common.IssueDeleteAPI
       )

handler :: FlowServer API
handler =
  createIssueReport
    :<|> issueReportDriverList
    :<|> issueMediaUpload
    :<|> fetchMedia
    :<|> getIssueCategory
    :<|> getIssueOption
    :<|> issueInfo
    :<|> updateIssueOption
    :<|> deleteIssue

issueReportDriverList :: Id SP.Person -> Maybe Language -> FlowHandler Common.IssueReportDriverListRes
issueReportDriverList driverId = withFlowHandlerAPI . Domain.issueReportDriverList driverId

fetchMedia :: Id SP.Person -> Text -> FlowHandler Text
fetchMedia driverId = withFlowHandlerAPI . Domain.fetchMedia driverId

createIssueReport :: Id SP.Person -> Common.IssueReportReq -> FlowHandler Common.IssueReportRes
createIssueReport driverId = withFlowHandlerAPI . Domain.createIssueReport driverId

issueMediaUpload :: Id SP.Person -> Common.IssueMediaUploadReq -> FlowHandler Common.IssueMediaUploadRes
issueMediaUpload driverId = withFlowHandlerAPI . Domain.issueMediaUpload driverId

issueInfo :: Id Domain.IssueReport -> Id SP.Person -> Maybe Language -> FlowHandler Common.IssueInfoRes
issueInfo issueReportId driverId = withFlowHandlerAPI . Domain.issueInfo issueReportId driverId

updateIssueOption :: Id Domain.IssueReport -> Id SP.Person -> Common.IssueUpdateReq -> FlowHandler APISuccess
updateIssueOption issueReportId driverId = withFlowHandlerAPI . Domain.updateIssueOption issueReportId driverId

deleteIssue :: Id Domain.IssueReport -> Id SP.Person -> FlowHandler APISuccess
deleteIssue issueReportId = withFlowHandlerAPI . Domain.deleteIssue issueReportId

getIssueCategory :: Id SP.Person -> Maybe Language -> FlowHandler Common.IssueCategoryListRes
getIssueCategory driverId = withFlowHandlerAPI . Domain.getIssueCategory driverId

getIssueOption :: Id SP.Person -> Id Common.IssueCategory -> Maybe Language -> FlowHandler Common.IssueOptionListRes
getIssueOption driverId issueCategoryId = withFlowHandlerAPI . Domain.getIssueOption driverId (cast @Common.IssueCategory @Domain.IssueCategory issueCategoryId)
