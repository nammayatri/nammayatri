{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Driver.Registration
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Registration as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Role as DRole
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations as Client
import qualified ProviderPlatformClient.DynamicOfferDriver.RideBooking as Client
import Servant
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Storage.Queries.Person as QP
import "lib-dashboard" Storage.Queries.Role as QRole
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant
import "lib-dashboard" Tools.Error

type API =
  "driver"
    :> ( DocumentsListAPI
           :<|> GetDocumentAPI
           :<|> UploadDocumentAPI
           :<|> RegisterDLAPI
           :<|> RegisterRCAPI
           :<|> GenerateAadhaarOtpAPI
           :<|> VerifyAadhaarOtpAPI
           :<|> AuthAPI
           :<|> VerifyAPI
       )

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  documentsList merchantId city
    :<|> getDocument merchantId city
    :<|> uploadDocument merchantId city
    :<|> registerDL merchantId city
    :<|> registerRC merchantId city
    :<|> generateAadhaarOtp merchantId city
    :<|> verifyAadhaarOtp merchantId city
    :<|> auth merchantId city
    :<|> verify merchantId city

type DocumentsListAPI = ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'DOCUMENT_LIST :> Common.DocumentsListAPI

type GetDocumentAPI = ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'GET_DOCUMENT :> Common.GetDocumentAPI

type UploadDocumentAPI = ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'UPLOAD_DOCUMENT :> Common.UploadDocumentAPI

type RegisterDLAPI = ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'REGISTER_DL :> Common.RegisterDLAPI

type RegisterRCAPI = ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'REGISTER_RC :> Common.RegisterRCAPI

type GenerateAadhaarOtpAPI = ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'GENERATE_AADHAAR_OTP :> Common.GenerateAadhaarOtpAPI

type VerifyAadhaarOtpAPI = ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'VERIFY_AADHAAR_OTP :> Common.VerifyAadhaarOtpAPI

type AuthAPI = ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'AUTH :> Common.AuthAPI

type VerifyAPI = ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'VERIFY :> Common.VerifyAPI

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
  T.buildTransaction (DT.DriverRegistrationAPI endpoint) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) (Just driverId) Nothing

documentsList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler Common.DocumentsListResponse
documentsList merchantShortId opCity apiTokenInfo driverId =
  withFlowHandlerAPI' $ do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverRegistration.documentsList) driverId

getDocument :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Image -> FlowHandler Common.GetDocumentResponse
getDocument merchantShortId opCity apiTokenInfo imageId =
  withFlowHandlerAPI' $ do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverRegistration.getDocument) imageId

uploadDocument :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UploadDocumentReq -> FlowHandler Common.UploadDocumentResp
uploadDocument merchantShortId opCity apiTokenInfo driverId req =
  withFlowHandlerAPI' $ do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
    transaction <- buildTransaction Common.UploadDocumentEndpoint apiTokenInfo driverId (Just req)
    T.withResponseTransactionStoring transaction $
      Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverRegistration.uploadDocument) driverId req

registerDL :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.RegisterDLReq -> FlowHandler APISuccess
registerDL merchantShortId opCity apiTokenInfo driverId req =
  withFlowHandlerAPI' $ do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
    transaction <- buildTransaction Common.RegisterDLEndpoint apiTokenInfo driverId (Just req)
    T.withTransactionStoring transaction $
      Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverRegistration.registerDL) driverId req

registerRC :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.RegisterRCReq -> FlowHandler APISuccess
registerRC merchantShortId opCity apiTokenInfo driverId req =
  withFlowHandlerAPI' $ do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
    transaction <- buildTransaction Common.RegisterRCEndpoint apiTokenInfo driverId (Just req)
    T.withTransactionStoring transaction $
      Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverRegistration.registerRC) driverId req

generateAadhaarOtp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.GenerateAadhaarOtpReq -> FlowHandler Common.GenerateAadhaarOtpRes
generateAadhaarOtp merchantShortId opCity apiTokenInfo driverId req =
  withFlowHandlerAPI' $ do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
    transaction <- buildTransaction Common.GenerateAadhaarOtpEndpoint apiTokenInfo driverId (Nothing :: Maybe Common.GenerateAadhaarOtpReq)
    T.withTransactionStoring transaction $
      Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverRegistration.generateAadhaarOtp) driverId req

verifyAadhaarOtp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.VerifyAadhaarOtpReq -> FlowHandler Common.VerifyAadhaarOtpRes
verifyAadhaarOtp merchantShortId opCity apiTokenInfo driverId req =
  withFlowHandlerAPI' $ do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
    transaction <- buildTransaction Common.VerifyAadhaarOtpEndpoint apiTokenInfo driverId (Nothing :: Maybe Common.VerifyAadhaarOtpReq)
    T.withTransactionStoring transaction $
      Client.callDriverOfferBPPOperations checkedMerchantId opCity (.drivers.driverRegistration.verifyAadhaarOtp) driverId req

auth :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.AuthReq -> FlowHandler Common.AuthRes
auth merchantShortId opCity apiTokenInfo req =
  withFlowHandlerAPI' $ do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
    Client.callDriverOfferBPP checkedMerchantId opCity (.driverRegistration.auth) req

verify :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Common.AuthVerifyReq -> FlowHandler APISuccess
verify merchantShortId opCity apiTokenInfo authId req =
  withFlowHandlerAPI' $ do
    checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
    encPerson <- QP.findById apiTokenInfo.personId >>= fromMaybeM (PersonNotFound apiTokenInfo.personId.getId)
    role <- QRole.findById encPerson.roleId >>= fromMaybeM (RoleNotFound encPerson.roleId.getId)
    let mbFleet = role.dashboardAccessType == DRole.FLEET_OWNER
    Client.callDriverOfferBPP checkedMerchantId opCity (.driverRegistration.verify) authId mbFleet apiTokenInfo.personId.getId req
