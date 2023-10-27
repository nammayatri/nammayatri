module API.UI.Issue where

import qualified Domain.Action.UI.Issue as Domain
import qualified Domain.Types.Issue.IssueCategory as Domain
import qualified Domain.Types.Issue.IssueReport as Domain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
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

issueReportDriverList :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Language -> FlowHandler Common.IssueReportDriverListRes
issueReportDriverList (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI . Domain.issueReportDriverList (driverId, merchantId, merchantOpCityId)

fetchMedia :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Text -> FlowHandler Text
fetchMedia (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI . Domain.fetchMedia (driverId, merchantId, merchantOpCityId)

createIssueReport :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Common.IssueReportReq -> FlowHandler Common.IssueReportRes
createIssueReport (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI . Domain.createIssueReport (driverId, merchantId, merchantOpCityId)

issueMediaUpload :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Common.IssueMediaUploadReq -> FlowHandler Common.IssueMediaUploadRes
issueMediaUpload (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI . Domain.issueMediaUpload (driverId, merchantId, merchantOpCityId)

issueInfo :: Id Domain.IssueReport -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Language -> FlowHandler Common.IssueInfoRes
issueInfo issueReportId (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI . Domain.issueInfo issueReportId (driverId, merchantId, merchantOpCityId)

updateIssueOption :: Id Domain.IssueReport -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Common.IssueUpdateReq -> FlowHandler APISuccess
updateIssueOption issueReportId (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI . Domain.updateIssueOption issueReportId (driverId, merchantId, merchantOpCityId)

deleteIssue :: Id Domain.IssueReport -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler APISuccess
deleteIssue issueReportId = withFlowHandlerAPI . Domain.deleteIssue issueReportId

getIssueCategory :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Language -> FlowHandler Common.IssueCategoryListRes
getIssueCategory (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI . Domain.getIssueCategory (driverId, merchantId, merchantOpCityId)

getIssueOption :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id Common.IssueCategory -> Maybe Language -> FlowHandler Common.IssueOptionListRes
getIssueOption (driverId, merchantId, merchantOpCityId) issueCategoryId = withFlowHandlerAPI . Domain.getIssueOption (driverId, merchantId, merchantOpCityId) (cast @Common.IssueCategory @Domain.IssueCategory issueCategoryId)
