 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Driver.Registration where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Registration as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
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
    Common.HideSecrets request
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
    transaction <- buildTransaction Common.UploadDocumentEndpoint apiTokenInfo driverId (Just req)
    T.withResponseTransactionStoring transaction $
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
