module API.BPP.DriverOffer.Driver.Registration where

import qualified BPPClient.DriverOffer as Client
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver.Registration as Common
import "lib-dashboard" Domain.Types.Person as DP
import "lib-dashboard" Environment
import Servant
import "lib-dashboard" Tools.Auth

type API =
  DocumentsListAPI
    :<|> GetDocumentAPI
    :<|> UploadDocumentAPI
    :<|> RegisterDLAPI
    :<|> RegisterRCAPI

handler :: FlowServer API
handler =
  documentsList
    :<|> getDocument
    :<|> uploadDocument
    :<|> registerDL
    :<|> registerRC

type DocumentsListAPI = ApiAuth 'READ_ACCESS 'DRIVERS :> Common.DocumentsListAPI

type GetDocumentAPI = ApiAuth 'READ_ACCESS 'DRIVERS :> Common.GetDocumentAPI

type UploadDocumentAPI = ApiAuth 'WRITE_ACCESS 'DRIVERS :> Common.UploadDocumentAPI

type RegisterDLAPI = ApiAuth 'WRITE_ACCESS 'DRIVERS :> Common.RegisterDLAPI

type RegisterRCAPI = ApiAuth 'WRITE_ACCESS 'DRIVERS :> Common.RegisterRCAPI

documentsList :: Id DP.Person -> Id Common.Driver -> FlowHandler Common.DocumentsListResponse
documentsList _ driverId =
  withFlowHandlerAPI $
    -- FIXME: drivers for only one organization?
    Client.callDriverOfferBPP (.drivers.documentsList) driverId

getDocument :: Id DP.Person -> Id Common.Image -> FlowHandler Common.GetDocumentResponse
getDocument _ imageId =
  withFlowHandlerAPI $
    -- FIXME: drivers for only one organization?
    Client.callDriverOfferBPP (.drivers.getDocument) imageId

uploadDocument :: Id DP.Person -> Id Common.Driver -> Common.UploadDocumentReq -> FlowHandler Common.UploadDocumentResp
uploadDocument _ driverId req =
  withFlowHandlerAPI $
    -- FIXME: drivers for only one organization?
    Client.callDriverOfferBPP (.drivers.uploadDocument) driverId req

registerDL :: Id DP.Person -> Id Common.Driver -> Common.RegisterDLReq -> FlowHandler APISuccess
registerDL _ driverId req =
  withFlowHandlerAPI $
    -- FIXME: drivers for only one organization?
    Client.callDriverOfferBPP (.drivers.registerDL) driverId req

registerRC :: Id DP.Person -> Id Common.Driver -> Common.RegisterRCReq -> FlowHandler APISuccess
registerRC _ driverId req =
  withFlowHandlerAPI $
    -- FIXME: drivers for only one organization?
    Client.callDriverOfferBPP (.drivers.registerRC) driverId req
