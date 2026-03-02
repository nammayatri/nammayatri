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
    postDriverRegistrationDocumentsCommon,
    postDriverRegistrationRegisterDl,
    postDriverRegistrationRegisterRc,
    postDriverRegistrationRegisterGenerateAadhaarOtp,
    postDriverRegistrationRegisterVerifyAadhaarOtp,
    getDriverRegistrationUnderReviewDrivers,
    getDriverRegistrationDocumentsInfo,
    postDriverRegistrationDocumentsUpdate,
    postDriverRegistrationRegisterAadhaar,
    postDriverRegistrationUnlinkDocument,
    getDriverRegistrationVerificationStatus,
    postDriverRegistrationTriggerReminder,
    postDriverRegistrationVerifyBankAccount,
    getDriverRegistrationInfoBankAccount,
    postDriverRegistrationDeleteBankAccount,
  )
where

import qualified API.Client.ProviderPlatform.Management as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import qualified Domain.Action.ProviderPlatform.Management.Account as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Person as DP
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.External.Verification.Interface.Types
import qualified Kernel.External.Verification.Types
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.Person as QP
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

-----------------------------------------------------------------------------------------------------------------------
----------------------------------------------- Helper Functions --------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

determineRequestorId :: ApiTokenInfo -> Id Common.Driver -> Maybe Text
determineRequestorId apiTokenInfo driverId =
  case apiTokenInfo.merchant.hasFleetMemberHierarchy of
    Just False ->
      if shouldExcludeRequestorId apiTokenInfo driverId
        then Nothing
        else Just apiTokenInfo.personId.getId
    _ -> Nothing

shouldExcludeRequestorId :: ApiTokenInfo -> Id Common.Driver -> Bool
shouldExcludeRequestorId apiTokenInfo driverId =
  DP.isAdmin apiTokenInfo.person || apiTokenInfo.personId.getId == driverId.getId

-----------------------------------------------------------------------------------------------------------------------
----------------------------------------------- Endpoints ------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

getDriverRegistrationDocumentsList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe Text -> Flow Common.DocumentsListResponse
getDriverRegistrationDocumentsList merchantShortId opCity apiTokenInfo driverId mbRcId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.getDriverRegistrationDocumentsList) driverId mbRcId

getDriverRegistrationGetDocument :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Image -> Flow Common.GetDocumentResponse
getDriverRegistrationGetDocument merchantShortId opCity apiTokenInfo imageId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.getDriverRegistrationGetDocument) imageId

postDriverRegistrationVerifyBankAccount :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.VerifyBankAccountReq -> Flow Kernel.External.Verification.Interface.Types.VerifyAsyncResp
postDriverRegistrationVerifyBankAccount merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationVerifyBankAccount) driverId req

getDriverRegistrationInfoBankAccount :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Kernel.Prelude.Text -> Flow Kernel.External.Verification.Types.BankAccountVerificationResponse
getDriverRegistrationInfoBankAccount merchantShortId opCity apiTokenInfo driverId requestId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.getDriverRegistrationInfoBankAccount) driverId requestId

postDriverRegistrationDeleteBankAccount :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverRegistrationDeleteBankAccount merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationDeleteBankAccount) driverId

postDriverRegistrationDocumentUpload :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UploadDocumentReq -> Flow Common.UploadDocumentResp
postDriverRegistrationDocumentUpload merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  let mbRequestorId = determineRequestorId apiTokenInfo driverId
  T.withResponseTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationDocumentUpload) driverId req {Common.requestorId = mbRequestorId}

postDriverRegistrationRegisterDl :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.RegisterDLReq -> Flow APISuccess
postDriverRegistrationRegisterDl merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  let mbAccessType = Common.castDashboardAccessType <$> apiTokenInfo.person.dashboardAccessType
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationRegisterDl) driverId req{accessType = mbAccessType}

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

postDriverRegistrationRegisterAadhaar :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.AadhaarCardReq -> Flow APISuccess
postDriverRegistrationRegisterAadhaar merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationRegisterAadhaar) driverId req

postDriverRegistrationUnlinkDocument :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.DocumentType -> Flow APISuccess
postDriverRegistrationUnlinkDocument merchantShortId opCity apiTokenInfo personId documentType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let mbRequestorId = determineRequestorId apiTokenInfo personId
  transaction <- buildTransaction apiTokenInfo (Just personId) (Just documentType)
  T.withTransactionStoring transaction $ do
    res <- Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationUnlinkDocument) personId documentType mbRequestorId
    when res.mandatoryDocumentRemoved $ do
      QP.updatePersonVerifiedStatus (cast personId) False
    pure Success

getDriverRegistrationVerificationStatus :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> UTCTime -> UTCTime -> Int -> Int -> Common.DocumentType -> Common.ServiceType -> Flow Common.VerificationStatusListResponse
getDriverRegistrationVerificationStatus merchantShortId opCity apiTokenInfo driverId fromDate toDate limit offset documentType serviceType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.getDriverRegistrationVerificationStatus) driverId fromDate toDate limit offset documentType serviceType

postDriverRegistrationDocumentsCommon ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Id Common.Driver ->
  Common.CommonDocumentCreateReq ->
  Flow APISuccess
postDriverRegistrationDocumentsCommon merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationDocumentsCommon) driverId req

postDriverRegistrationTriggerReminder :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.TriggerReminderReq -> Flow APISuccess
postDriverRegistrationTriggerReminder merchantShortId opCity apiTokenInfo driverId req = do
  runRequestValidation Common.validateTriggerReminderReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  let mbRequestorId = determineRequestorId apiTokenInfo driverId
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationTriggerReminder) driverId mbRequestorId req

--- << AUTOGENERATED Check this code, update export list and remove comment >> ---
