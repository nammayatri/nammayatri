module API.UI.Issue where

import qualified "shared-services" SharedService.ProviderPlatform.Issue as Common
import qualified Domain.Action.UI.Issue as Domain
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Issue.IssueReport as Domain
import qualified Domain.Types.Issue.IssueCategory as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.External.Types (Language)
import Servant
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
         :<|> Capture "issueId" (Id Domain.IssueReport)
            :> "update"
            :> TokenAuth
            :> Common.IssueUpdateAPI
         :<|> Capture "issueId" (Id Domain.IssueReport)
            :> "delete"
            :> TokenAuth
            :> Common.IssueDeleteAPI
         :<|> "category"
            :> TokenAuth
            :> Common.IssueCategoryAPI
         :<|> "option"
            :> TokenAuth
            :> Common.IssueOptionAPI
        )

handler :: FlowServer API
handler =
   createIssueReport
      :<|> issueReportDriverList
      :<|> issueMediaUpload
      :<|> fetchMedia
      :<|> updateIssue
      :<|> deleteIssue
      :<|> getIssueCategory
      :<|> getIssueOption

issueReportDriverList :: Id SP.Person -> FlowHandler Common.IssueReportDriverListRes
issueReportDriverList = withFlowHandlerAPI . Domain.issueReportDriverList

fetchMedia :: Id SP.Person -> Text -> FlowHandler Text
fetchMedia driverId = withFlowHandlerAPI . Domain.fetchMedia driverId

createIssueReport :: Id SP.Person -> Common.IssueReportReq -> FlowHandler Common.IssueReportRes
createIssueReport driverId = withFlowHandlerAPI . Domain.createIssueReport driverId

issueMediaUpload :: Id SP.Person -> Common.IssueMediaUploadReq -> FlowHandler Common.IssueMediaUploadRes
issueMediaUpload driverId = withFlowHandlerAPI . Domain.issueMediaUpload driverId

updateIssue :: Id Domain.IssueReport -> Id SP.Person -> Common.IssueUpdateReq -> FlowHandler APISuccess
updateIssue issueReportId driverId = withFlowHandlerAPI . Domain.updateIssue issueReportId driverId

deleteIssue :: Id Domain.IssueReport -> Id SP.Person -> FlowHandler APISuccess
deleteIssue issueReportId = withFlowHandlerAPI . Domain.deleteIssue issueReportId

getIssueCategory :: Id SP.Person -> Maybe Language -> FlowHandler Common.IssueCategoryListRes
getIssueCategory driverId = withFlowHandlerAPI . Domain.getIssueCategory driverId

getIssueOption :: Id SP.Person -> Id Common.IssueCategory -> Maybe Language -> FlowHandler Common.IssueOptionListRes
getIssueOption driverId issueCategoryId = withFlowHandlerAPI . Domain.getIssueOption driverId (cast @Common.IssueCategory @Domain.IssueCategory issueCategoryId)