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
    getDriverRegistrationPayoutRegistration,
    getDriverRegistrationPayoutOrderStatus,
    postDriverRegistrationDeleteBankAccount,
    getDriverRegistrationDocumentsCommonList,
    postDriverRegistrationDocumentRegister,
    -- Audit-actor helpers shared with the Management.Driver and RideBooking.Driver handlers (forwarded to the BPP
    -- so document mutations triggered from the dashboard are attributed to the operator, gated per merchant).
    determineAuditRequestorId,
    determineAuditRequestorRole,
  )
where

import qualified API.Client.ProviderPlatform.Management as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import qualified Domain.Action.ProviderPlatform.Management.Account as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Person as DP
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import qualified Kernel.External.Payout.Interface.Types as PayoutTypes
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
import qualified Storage.Queries.Role as QRole
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

-- For document-audit attribution: forward the acting user's id only when the merchant has opted in
-- via sendDocumentAuditActorDetails. The BPP attributes the DocumentAuditLog to this requestor.
determineAuditRequestorId :: ApiTokenInfo -> Maybe Text
determineAuditRequestorId apiTokenInfo =
  case apiTokenInfo.merchant.sendDocumentAuditActorDetails of
    Just True -> Just apiTokenInfo.personId.getId
    _ -> Nothing

-- The acting user's actual role name (from the role table), forwarded alongside the id so the BPP records the
-- real role. Queried only when the merchant opted in via sendDocumentAuditActorDetails; best-effort (a missing
-- role yields Nothing rather than failing the request). role.name is free-form so custom dashboard roles flow through.
determineAuditRequestorRole :: ApiTokenInfo -> Flow (Maybe Text)
determineAuditRequestorRole apiTokenInfo =
  case apiTokenInfo.merchant.sendDocumentAuditActorDetails of
    Just True -> QRole.findById apiTokenInfo.person.roleId <&> fmap (.name)
    _ -> pure Nothing

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
  -- Audit actor id + role, both gated on merchant.sendDocumentAuditActorDetails, so the BPP attributes the bank
  -- verification to the dashboard operator instead of the driver.
  let mbRequestorId = determineAuditRequestorId apiTokenInfo
  mbRequestorRole <- determineAuditRequestorRole apiTokenInfo
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationVerifyBankAccount) driverId (req {Common.requestorId = mbRequestorId, Common.requestorRole = mbRequestorRole} :: Common.VerifyBankAccountReq)

getDriverRegistrationInfoBankAccount :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Kernel.Prelude.Text -> Flow Kernel.External.Verification.Types.BankAccountVerificationResponse
getDriverRegistrationInfoBankAccount merchantShortId opCity apiTokenInfo driverId requestId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.getDriverRegistrationInfoBankAccount) driverId requestId

postDriverRegistrationDeleteBankAccount :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverRegistrationDeleteBankAccount merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  -- Bodyless endpoint: forward the audit actor (id + role) on the internal helperApi so the BPP attributes the
  -- bank-account delete to the dashboard operator. Both gated on merchant.sendDocumentAuditActorDetails; the
  -- frontend sends nothing (these are derived from the auth token here).
  let mbRequestorId = determineAuditRequestorId apiTokenInfo
  mbRequestorRole <- determineAuditRequestorRole apiTokenInfo
  transaction <- buildTransaction apiTokenInfo (Just driverId) T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationDeleteBankAccount) driverId mbRequestorId mbRequestorRole

postDriverRegistrationDocumentUpload :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UploadDocumentReq -> Flow Common.UploadDocumentResp
postDriverRegistrationDocumentUpload merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  let mbRequestorId = determineRequestorId apiTokenInfo driverId
  -- auditRequestorId is the audit ACTOR id (the acting user, forwarded when the merchant opted in), kept separate
  -- from requestorId which drives the access checks and is Nothing for self-uploads. requestorRole is the role.
  let mbAuditRequestorId = determineAuditRequestorId apiTokenInfo
  mbRequestorRole <- determineAuditRequestorRole apiTokenInfo
  T.withResponseTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationDocumentUpload) driverId (req {Common.requestorId = mbRequestorId, Common.auditRequestorId = mbAuditRequestorId, Common.requestorRole = mbRequestorRole} :: Common.UploadDocumentReq)

postDriverRegistrationRegisterDl :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.RegisterDLReq -> Flow APISuccess
postDriverRegistrationRegisterDl merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  let mbAccessType = Common.castDashboardAccessType <$> apiTokenInfo.person.dashboardAccessType
  -- Audit actor id + role, both gated on merchant.sendDocumentAuditActorDetails, so the BPP attributes the DL
  -- verification to the dashboard operator instead of the driver.
  let mbRequestorId = determineAuditRequestorId apiTokenInfo
  mbRequestorRole <- determineAuditRequestorRole apiTokenInfo
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationRegisterDl) driverId (req {Common.accessType = mbAccessType, Common.requestorId = mbRequestorId, Common.requestorRole = mbRequestorRole} :: Common.RegisterDLReq)

postDriverRegistrationRegisterRc :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.RegisterRCReq -> Flow APISuccess
postDriverRegistrationRegisterRc merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  -- Audit actor id + role, both gated on merchant.sendDocumentAuditActorDetails, so the BPP attributes the RC
  -- verification to the dashboard operator instead of the driver.
  let mbRequestorId = determineAuditRequestorId apiTokenInfo
  mbRequestorRole <- determineAuditRequestorRole apiTokenInfo
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationRegisterRc) driverId (req {Common.requestorId = mbRequestorId, Common.requestorRole = mbRequestorRole} :: Common.RegisterRCReq)

postDriverRegistrationRegisterGenerateAadhaarOtp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.GenerateAadhaarOtpReq -> Flow Common.GenerateAadhaarOtpRes
postDriverRegistrationRegisterGenerateAadhaarOtp merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Nothing :: Maybe Common.GenerateAadhaarOtpReq)
  -- Audit actor id + role, both gated on merchant.sendDocumentAuditActorDetails, so the BPP attributes the
  -- generate-OTP request to the dashboard operator instead of the driver.
  let mbRequestorId = determineAuditRequestorId apiTokenInfo
  mbRequestorRole <- determineAuditRequestorRole apiTokenInfo
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationRegisterGenerateAadhaarOtp) driverId (req {Common.requestorId = mbRequestorId, Common.requestorRole = mbRequestorRole} :: Common.GenerateAadhaarOtpReq)

postDriverRegistrationRegisterVerifyAadhaarOtp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.VerifyAadhaarOtpReq -> Flow Common.VerifyAadhaarOtpRes
postDriverRegistrationRegisterVerifyAadhaarOtp merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Nothing :: Maybe Common.VerifyAadhaarOtpReq)
  -- Audit actor id + role, both gated on merchant.sendDocumentAuditActorDetails, so the BPP attributes the
  -- Aadhaar OTP verification to the dashboard operator instead of the driver.
  let mbRequestorId = determineAuditRequestorId apiTokenInfo
  mbRequestorRole <- determineAuditRequestorRole apiTokenInfo
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationRegisterVerifyAadhaarOtp) driverId (req {Common.requestorId = mbRequestorId, Common.requestorRole = mbRequestorRole} :: Common.VerifyAadhaarOtpReq)

getDriverRegistrationUnderReviewDrivers :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Flow Common.UnderReviewDriversListResponse
getDriverRegistrationUnderReviewDrivers merchantShortId opCity apiTokenInfo limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.getDriverRegistrationUnderReviewDrivers) limit offset

getDriverRegistrationDocumentsInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow Common.StatusRes
getDriverRegistrationDocumentsInfo merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.getDriverRegistrationDocumentsInfo) driverId

postDriverRegistrationDocumentsUpdate :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.UpdateDocumentRequest -> Flow Common.UpdateDocumentResp
postDriverRegistrationDocumentsUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let mbRequestorId = determineAuditRequestorId apiTokenInfo
  mbRequestorRole <- determineAuditRequestorRole apiTokenInfo
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $ do
    res <- Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationDocumentsUpdate) mbRequestorId mbRequestorRole req
    whenJust res.personId $ \personId ->
      QP.updatePersonVerifiedStatus (cast personId) res.enabled
    pure res

postDriverRegistrationRegisterAadhaar :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.AadhaarCardReq -> Flow APISuccess
postDriverRegistrationRegisterAadhaar merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  -- Audit actor id + role, both gated on merchant.sendDocumentAuditActorDetails, so the BPP attributes the
  -- Aadhaar-card registration to the dashboard operator instead of the driver.
  let mbRequestorId = determineAuditRequestorId apiTokenInfo
  mbRequestorRole <- determineAuditRequestorRole apiTokenInfo
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationRegisterAadhaar) driverId (req {Common.requestorId = mbRequestorId, Common.requestorRole = mbRequestorRole} :: Common.AadhaarCardReq)

postDriverRegistrationUnlinkDocument :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.DocumentType -> Flow APISuccess
postDriverRegistrationUnlinkDocument merchantShortId opCity apiTokenInfo personId documentType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  -- Audit actor id + role are BOTH gated on the merchant's sendDocumentAuditActorDetails flag (same as
  -- documents/update). When the flag is off, neither is forwarded — so no actor details leave the dashboard.
  let mbRequestorId = determineAuditRequestorId apiTokenInfo
  mbRequestorRole <- determineAuditRequestorRole apiTokenInfo
  transaction <- buildTransaction apiTokenInfo (Just personId) (Just documentType)
  T.withTransactionStoring transaction $ do
    res <- Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationUnlinkDocument) personId documentType mbRequestorId mbRequestorRole
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
  Flow Common.CommonDocumentCreateRes
postDriverRegistrationDocumentsCommon merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  -- Audit actor id + role, both gated on merchant.sendDocumentAuditActorDetails, so the BPP attributes the
  -- common-document creation to the dashboard operator instead of the driver.
  let mbRequestorId = determineAuditRequestorId apiTokenInfo
  mbRequestorRole <- determineAuditRequestorRole apiTokenInfo
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationDocumentsCommon) driverId (req {Common.requestorId = mbRequestorId, Common.requestorRole = mbRequestorRole} :: Common.CommonDocumentCreateReq)

postDriverRegistrationTriggerReminder :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.TriggerReminderReq -> Flow APISuccess
postDriverRegistrationTriggerReminder merchantShortId opCity apiTokenInfo driverId req = do
  runRequestValidation Common.validateTriggerReminderReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  let mbRequestorId = determineRequestorId apiTokenInfo driverId
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationTriggerReminder) driverId mbRequestorId req

getDriverRegistrationPayoutRegistration :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow Common.PayoutRegistrationRes
getDriverRegistrationPayoutRegistration merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.getDriverRegistrationPayoutRegistration) driverId (Just apiTokenInfo.personId.getId)

getDriverRegistrationPayoutOrderStatus :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Text -> Flow PayoutTypes.PayoutOrderStatusResp
getDriverRegistrationPayoutOrderStatus merchantShortId opCity apiTokenInfo driverId orderId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.getDriverRegistrationPayoutOrderStatus) driverId orderId

getDriverRegistrationDocumentsCommonList ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe [Common.DocumentType] ->
  Maybe [Common.VerificationStatus] ->
  Maybe [Id Common.Driver] ->
  Maybe Text ->
  Maybe Text ->
  Flow Common.CommonDocumentsListRes
getDriverRegistrationDocumentsCommonList merchantShortId opCity apiTokenInfo limit offset from to documentType verificationStatus driverId sortByField sortOrder = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.getDriverRegistrationDocumentsCommonList) limit offset from to documentType verificationStatus driverId sortByField sortOrder

postDriverRegistrationDocumentRegister :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.DocumentRegisterReq -> Flow APISuccess
postDriverRegistrationDocumentRegister merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $ Client.callManagementAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationDocumentRegister) driverId req
