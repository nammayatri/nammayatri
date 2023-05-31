module API.UI.Issue where

import qualified Domain.Action.UI.Issue as Domain
import qualified Domain.Types.Issue.IssueCategory as Domain
import qualified Domain.Types.Issue.IssueReport as Domain
import qualified Domain.Types.Merchant as DM
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

issueReportDriverList :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> FlowHandler Common.IssueReportDriverListRes
issueReportDriverList (driverId, merchantId) = withFlowHandlerAPI . Domain.issueReportDriverList (driverId, merchantId)

fetchMedia :: (Id SP.Person, Id DM.Merchant) -> Text -> FlowHandler Text
fetchMedia (driverId, merchantId) = withFlowHandlerAPI . Domain.fetchMedia (driverId, merchantId)

createIssueReport :: (Id SP.Person, Id DM.Merchant) -> Common.IssueReportReq -> FlowHandler Common.IssueReportRes
createIssueReport (driverId, merchantId) = withFlowHandlerAPI . Domain.createIssueReport (driverId, merchantId)

issueMediaUpload :: (Id SP.Person, Id DM.Merchant) -> Common.IssueMediaUploadReq -> FlowHandler Common.IssueMediaUploadRes
issueMediaUpload (driverId, merchantId) = withFlowHandlerAPI . Domain.issueMediaUpload (driverId, merchantId)

issueInfo :: Id Domain.IssueReport -> (Id SP.Person, Id DM.Merchant) -> Maybe Language -> FlowHandler Common.IssueInfoRes
issueInfo issueReportId (driverId, merchantId) = withFlowHandlerAPI . Domain.issueInfo issueReportId (driverId, merchantId)

updateIssueOption :: Id Domain.IssueReport -> (Id SP.Person, Id DM.Merchant) -> Common.IssueUpdateReq -> FlowHandler APISuccess
updateIssueOption issueReportId (driverId, merchantId) = withFlowHandlerAPI . Domain.updateIssueOption issueReportId (driverId, merchantId)

deleteIssue :: Id Domain.IssueReport -> (Id SP.Person, Id DM.Merchant) -> FlowHandler APISuccess
deleteIssue issueReportId = withFlowHandlerAPI . Domain.deleteIssue issueReportId

getIssueCategory :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> FlowHandler Common.IssueCategoryListRes
getIssueCategory (driverId, merchantId) = withFlowHandlerAPI . Domain.getIssueCategory (driverId, merchantId)

getIssueOption :: (Id SP.Person, Id DM.Merchant) -> Id Common.IssueCategory -> Maybe Language -> FlowHandler Common.IssueOptionListRes
getIssueOption (driverId, merchantId) issueCategoryId = withFlowHandlerAPI . Domain.getIssueOption (driverId, merchantId) (cast @Common.IssueCategory @Domain.IssueCategory issueCategoryId)
