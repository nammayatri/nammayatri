module API.Dashboard.Driver.Registration where

import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified "dashboard-bpp-helper-api" Dashboard.BPP.Driver.Registration as Common
import qualified Domain.Action.Dashboard.Driver.Registration as DReg
import qualified Domain.Types.Merchant as DM
import Environment
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
