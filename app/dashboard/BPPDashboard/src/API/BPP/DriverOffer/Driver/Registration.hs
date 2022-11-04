module API.BPP.DriverOffer.Driver.Registration where

import qualified BPPClient.DriverOffer as Client
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver.Registration as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DMerchant
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

type DocumentsListAPI = ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'DRIVERS :> Common.DocumentsListAPI

type GetDocumentAPI = ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'DRIVERS :> Common.GetDocumentAPI

type UploadDocumentAPI = ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'DRIVERS :> Common.UploadDocumentAPI

type RegisterDLAPI = ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'DRIVERS :> Common.RegisterDLAPI

type RegisterRCAPI = ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'DRIVERS :> Common.RegisterRCAPI

documentsList :: ShortId DMerchant.Merchant -> Id Common.Driver -> FlowHandler Common.DocumentsListResponse
documentsList _ driverId =
  withFlowHandlerAPI $
    -- FIXME: drivers for only one organization?
    Client.callDriverOfferBPP (.drivers.documentsList) driverId

getDocument :: ShortId DMerchant.Merchant -> Id Common.Image -> FlowHandler Common.GetDocumentResponse
getDocument _ imageId =
  withFlowHandlerAPI $
    -- FIXME: drivers for only one organization?
    Client.callDriverOfferBPP (.drivers.getDocument) imageId

uploadDocument :: ShortId DMerchant.Merchant -> Id Common.Driver -> Common.UploadDocumentReq -> FlowHandler Common.UploadDocumentResp
uploadDocument _ driverId req =
  withFlowHandlerAPI $
    -- FIXME: drivers for only one organization?
    Client.callDriverOfferBPP (.drivers.uploadDocument) driverId req

registerDL :: ShortId DMerchant.Merchant -> Id Common.Driver -> Common.RegisterDLReq -> FlowHandler APISuccess
registerDL _ driverId req =
  withFlowHandlerAPI $
    -- FIXME: drivers for only one organization?
    Client.callDriverOfferBPP (.drivers.registerDL) driverId req

registerRC :: ShortId DMerchant.Merchant -> Id Common.Driver -> Common.RegisterRCReq -> FlowHandler APISuccess
registerRC _ driverId req =
  withFlowHandlerAPI $
    -- FIXME: drivers for only one organization?
    Client.callDriverOfferBPP (.drivers.registerRC) driverId req
