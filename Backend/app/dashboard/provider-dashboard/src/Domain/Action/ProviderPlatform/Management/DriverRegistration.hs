{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.ProviderPlatform.Management.DriverRegistration
  ( getDriverRegistrationDocumentsList,
    getDriverRegistrationGetDocument,
    postDriverRegistrationDocumentUpload,
    postDriverRegistrationRegisterDl,
    postDriverRegistrationRegisterRc,
    postDriverRegistrationRegisterGenerateAadhaarOtp,
    postDriverRegistrationRegisterVerifyAadhaarOtp,
    getDriverRegistrationUnderReviewDrivers,
    getDriverRegistrationDocumentsInfo,
    postDriverRegistrationDocumentsUpdate,
  )
where

import qualified API.Client.ProviderPlatform.Management as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  ApiTokenInfo ->
  Maybe (Id Common.Driver) ->
  Maybe request ->
  m DT.Transaction
buildTransaction apiTokenInfo driverId =
  T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) driverId Nothing

getDriverRegistrationDocumentsList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow Common.DocumentsListResponse
getDriverRegistrationDocumentsList merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.getDriverRegistrationDocumentsList) driverId

getDriverRegistrationGetDocument :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Image -> Flow Common.GetDocumentResponse
getDriverRegistrationGetDocument merchantShortId opCity apiTokenInfo imageId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.getDriverRegistrationGetDocument) imageId

postDriverRegistrationDocumentUpload :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UploadDocumentReq -> Flow Common.UploadDocumentResp
postDriverRegistrationDocumentUpload merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationDocumentUpload) driverId req

postDriverRegistrationRegisterDl :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.RegisterDLReq -> Flow APISuccess
postDriverRegistrationRegisterDl merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationRegisterDl) driverId req

postDriverRegistrationRegisterRc :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.RegisterRCReq -> Flow APISuccess
postDriverRegistrationRegisterRc merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationRegisterRc) driverId req

postDriverRegistrationRegisterGenerateAadhaarOtp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.GenerateAadhaarOtpReq -> Flow Common.GenerateAadhaarOtpRes
postDriverRegistrationRegisterGenerateAadhaarOtp merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Nothing :: Maybe Common.GenerateAadhaarOtpReq)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationRegisterGenerateAadhaarOtp) driverId req

postDriverRegistrationRegisterVerifyAadhaarOtp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.VerifyAadhaarOtpReq -> Flow Common.VerifyAadhaarOtpRes
postDriverRegistrationRegisterVerifyAadhaarOtp merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Nothing :: Maybe Common.VerifyAadhaarOtpReq)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationRegisterVerifyAadhaarOtp) driverId req

getDriverRegistrationUnderReviewDrivers :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Flow Common.UnderReviewDriversListResponse
getDriverRegistrationUnderReviewDrivers merchantShortId opCity apiTokenInfo limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.getDriverRegistrationUnderReviewDrivers) limit offset

getDriverRegistrationDocumentsInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow [Common.DriverDocument]
getDriverRegistrationDocumentsInfo merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.getDriverRegistrationDocumentsInfo) driverId

postDriverRegistrationDocumentsUpdate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.UpdateDocumentRequest -> Flow APISuccess
postDriverRegistrationDocumentsUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationDocumentsUpdate) req
