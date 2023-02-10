module API.Dashboard.Driver.Registration where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Registration as Common
import qualified Domain.Action.Dashboard.Driver.Registration as DReg
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  Common.DocumentsListAPI
    :<|> Common.GetDocumentAPI
    :<|> Common.UploadDocumentAPI
    :<|> Common.RegisterDLAPI
    :<|> Common.RegisterRCAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  documentsList merchantId
    :<|> getDocument merchantId
    :<|> uploadDocument merchantId
    :<|> registerDL merchantId
    :<|> registerRC merchantId

documentsList :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler Common.DocumentsListResponse
documentsList merchantShortId = withFlowHandlerAPI . DReg.documentsList merchantShortId

getDocument :: ShortId DM.Merchant -> Id Common.Image -> FlowHandler Common.GetDocumentResponse
getDocument merchantShortId = withFlowHandlerAPI . DReg.getDocument merchantShortId

uploadDocument :: ShortId DM.Merchant -> Id Common.Driver -> Common.UploadDocumentReq -> FlowHandler Common.UploadDocumentResp
uploadDocument merchantShortId driverId_ = withFlowHandlerAPI . DReg.uploadDocument merchantShortId driverId_

registerDL :: ShortId DM.Merchant -> Id Common.Driver -> Common.RegisterDLReq -> FlowHandler APISuccess
registerDL merchantShortId driverId_ = withFlowHandlerAPI . DReg.registerDL merchantShortId driverId_

registerRC :: ShortId DM.Merchant -> Id Common.Driver -> Common.RegisterRCReq -> FlowHandler APISuccess
registerRC merchantShortId driverId_ = withFlowHandlerAPI . DReg.registerRC merchantShortId driverId_
