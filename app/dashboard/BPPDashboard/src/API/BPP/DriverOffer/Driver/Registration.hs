module API.BPP.DriverOffer.Driver.Registration where

import qualified BPPClient.DriverOffer as Client
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver.Registration as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Servant
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth hiding (BECKN_TRANSPORT)
import "lib-dashboard" Tools.Auth.Merchant

type API =
  DocumentsListAPI
    :<|> GetDocumentAPI
    :<|> UploadDocumentAPI
    :<|> RegisterDLAPI
    :<|> RegisterRCAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  documentsList merchantId
    :<|> getDocument merchantId
    :<|> uploadDocument merchantId
    :<|> registerDL merchantId
    :<|> registerRC merchantId

type DocumentsListAPI = ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'DRIVERS :> Common.DocumentsListAPI

type GetDocumentAPI = ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'DRIVERS :> Common.GetDocumentAPI

type UploadDocumentAPI = ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'DRIVERS :> Common.UploadDocumentAPI

type RegisterDLAPI = ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'DRIVERS :> Common.RegisterDLAPI

type RegisterRCAPI = ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'DRIVERS :> Common.RegisterRCAPI

buildTransaction ::
  ( MonadFlow m,
    ToJSON request
  ) =>
  Common.DriverRegistrationEndpoint ->
  ApiTokenInfo ->
  Id Common.Driver ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo driverId =
  T.buildTransaction (DT.DriverRegistrationAPI endpoint) apiTokenInfo (Just driverId) Nothing

documentsList :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler Common.DocumentsListResponse
documentsList merchantShortId apiTokenInfo driverId =
  withFlowHandlerAPI $ do
    checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
    Client.callDriverOfferBPP checkedMerchantId (.drivers.documentsList) driverId

getDocument :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Image -> FlowHandler Common.GetDocumentResponse
getDocument merchantShortId apiTokenInfo imageId =
  withFlowHandlerAPI $ do
    checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
    Client.callDriverOfferBPP checkedMerchantId (.drivers.getDocument) imageId

uploadDocument :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> Common.UploadDocumentReq -> FlowHandler Common.UploadDocumentResp
uploadDocument merchantShortId apiTokenInfo driverId req =
  withFlowHandlerAPI $ do
    checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
    let transactionReq = Common.UploadDocumentTransactionReq req.imageType
    transaction <- buildTransaction Common.UploadDocumentEndpoint apiTokenInfo driverId (Just transactionReq)
    T.withResponseTransactionStoring (Just . identity) transaction $
      Client.callDriverOfferBPP checkedMerchantId (.drivers.uploadDocument) driverId req

registerDL :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> Common.RegisterDLReq -> FlowHandler APISuccess
registerDL merchantShortId apiTokenInfo driverId req =
  withFlowHandlerAPI $ do
    checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
    transaction <- buildTransaction Common.RegisterDLEndpoint apiTokenInfo driverId (Just req)
    T.withTransactionStoring transaction $
      Client.callDriverOfferBPP checkedMerchantId (.drivers.registerDL) driverId req

registerRC :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Driver -> Common.RegisterRCReq -> FlowHandler APISuccess
registerRC merchantShortId apiTokenInfo driverId req =
  withFlowHandlerAPI $ do
    checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
    transaction <- buildTransaction Common.RegisterRCEndpoint apiTokenInfo driverId (Just req)
    T.withTransactionStoring transaction $
      Client.callDriverOfferBPP checkedMerchantId (.drivers.registerRC) driverId req
