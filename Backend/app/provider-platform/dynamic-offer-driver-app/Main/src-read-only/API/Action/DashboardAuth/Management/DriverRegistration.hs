{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.DriverRegistration
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.DriverRegistration
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.DriverRegistration
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Payout.Interface.Types
import qualified Kernel.External.Verification.Interface.Types
import qualified Kernel.External.Verification.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("driver" :> (GetDriverRegistrationDocumentsList :<|> GetDriverRegistrationGetDocument :<|> PostDriverRegistrationGenerateTempAppCode :<|> PostDriverRegistrationDocumentUpload :<|> PostDriverRegistrationRegisterDl :<|> PostDriverRegistrationVerifyBankAccount :<|> GetDriverRegistrationInfoBankAccount :<|> GetDriverRegistrationPayoutRegistration :<|> PostDriverRegistrationDeleteBankAccount :<|> GetDriverRegistrationPayoutOrderStatus :<|> PostDriverRegistrationRegisterRc :<|> PostDriverRegistrationRegisterAadhaar :<|> PostDriverRegistrationRegisterGenerateAadhaarOtp :<|> PostDriverRegistrationRegisterVerifyAadhaarOtp :<|> GetDriverRegistrationUnderReviewDrivers :<|> GetDriverRegistrationDocumentsInfo :<|> GetDriverRegistrationVerificationStatus :<|> GetDriverRegistrationDocumentsCommonList :<|> PostDriverRegistrationDocumentsUpdate :<|> PostDriverRegistrationDocumentsCommon :<|> PostDriverRegistrationDocumentRegister :<|> PostDriverRegistrationUnlinkDocument :<|> PostDriverRegistrationTriggerReminder))

type GetDriverRegistrationDocumentsList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_DOCUMENTS_LIST"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.GetDriverRegistrationDocumentsList
  )

type GetDriverRegistrationGetDocument =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_GET_DOCUMENT"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.GetDriverRegistrationGetDocument
  )

type PostDriverRegistrationGenerateTempAppCode =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_GENERATE_TEMP_APP_CODE"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationGenerateTempAppCode
  )

type PostDriverRegistrationDocumentUpload =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DOCUMENT_UPLOAD"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationDocumentUpload
  )

type PostDriverRegistrationRegisterDl =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_DL"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationRegisterDl
  )

type PostDriverRegistrationVerifyBankAccount =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_VERIFY_BANK_ACCOUNT"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationVerifyBankAccount
  )

type GetDriverRegistrationInfoBankAccount =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_INFO_BANK_ACCOUNT"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.GetDriverRegistrationInfoBankAccount
  )

type GetDriverRegistrationPayoutRegistration =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_PAYOUT_REGISTRATION"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.GetDriverRegistrationPayoutRegistration
  )

type PostDriverRegistrationDeleteBankAccount =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DELETE_BANK_ACCOUNT"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationDeleteBankAccount
  )

type GetDriverRegistrationPayoutOrderStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_PAYOUT_ORDER_STATUS"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.GetDriverRegistrationPayoutOrderStatus
  )

type PostDriverRegistrationRegisterRc =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_RC"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationRegisterRc
  )

type PostDriverRegistrationRegisterAadhaar =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_AADHAAR"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationRegisterAadhaar
  )

type PostDriverRegistrationRegisterGenerateAadhaarOtp =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_GENERATE_AADHAAR_OTP"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationRegisterGenerateAadhaarOtp
  )

type PostDriverRegistrationRegisterVerifyAadhaarOtp =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_VERIFY_AADHAAR_OTP"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationRegisterVerifyAadhaarOtp
  )

type GetDriverRegistrationUnderReviewDrivers =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_UNDER_REVIEW_DRIVERS"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.GetDriverRegistrationUnderReviewDrivers
  )

type GetDriverRegistrationDocumentsInfo =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_DOCUMENTS_INFO"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.GetDriverRegistrationDocumentsInfo
  )

type GetDriverRegistrationVerificationStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_VERIFICATION_STATUS"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.GetDriverRegistrationVerificationStatus
  )

type GetDriverRegistrationDocumentsCommonList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_DOCUMENTS_COMMON_LIST"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.GetDriverRegistrationDocumentsCommonList
  )

type PostDriverRegistrationDocumentsUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DOCUMENTS_UPDATE"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationDocumentsUpdate
  )

type PostDriverRegistrationDocumentsCommon =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DOCUMENTS_COMMON"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationDocumentsCommon
  )

type PostDriverRegistrationDocumentRegister =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DOCUMENT_REGISTER"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationDocumentRegister
  )

type PostDriverRegistrationUnlinkDocument =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_UNLINK_DOCUMENT"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationUnlinkDocument
  )

type PostDriverRegistrationTriggerReminder =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_TRIGGER_REMINDER"
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationTriggerReminder
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverRegistrationDocumentsList merchantId city :<|> getDriverRegistrationGetDocument merchantId city :<|> postDriverRegistrationGenerateTempAppCode merchantId city :<|> postDriverRegistrationDocumentUpload merchantId city :<|> postDriverRegistrationRegisterDl merchantId city :<|> postDriverRegistrationVerifyBankAccount merchantId city :<|> getDriverRegistrationInfoBankAccount merchantId city :<|> getDriverRegistrationPayoutRegistration merchantId city :<|> postDriverRegistrationDeleteBankAccount merchantId city :<|> getDriverRegistrationPayoutOrderStatus merchantId city :<|> postDriverRegistrationRegisterRc merchantId city :<|> postDriverRegistrationRegisterAadhaar merchantId city :<|> postDriverRegistrationRegisterGenerateAadhaarOtp merchantId city :<|> postDriverRegistrationRegisterVerifyAadhaarOtp merchantId city :<|> getDriverRegistrationUnderReviewDrivers merchantId city :<|> getDriverRegistrationDocumentsInfo merchantId city :<|> getDriverRegistrationVerificationStatus merchantId city :<|> getDriverRegistrationDocumentsCommonList merchantId city :<|> postDriverRegistrationDocumentsUpdate merchantId city :<|> postDriverRegistrationDocumentsCommon merchantId city :<|> postDriverRegistrationDocumentRegister merchantId city :<|> postDriverRegistrationUnlinkDocument merchantId city :<|> postDriverRegistrationTriggerReminder merchantId city

getDriverRegistrationDocumentsList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.DriverRegistration.DocumentType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.DocumentsListResponse)
getDriverRegistrationDocumentsList a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationDocumentsList a6 a5 a3 a2 a1

getDriverRegistrationGetDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.DriverRegistration.DocumentType) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.DriverRegistration.EntityType) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.GetDocumentResponse)
getDriverRegistrationGetDocument a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationGetDocument a6 a5 a3 a2 a1

postDriverRegistrationGenerateTempAppCode :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.TempAppCodeRes)
postDriverRegistrationGenerateTempAppCode a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationGenerateTempAppCode a4 a3 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)

postDriverRegistrationDocumentUpload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.UploadDocumentReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.UploadDocumentResp)
postDriverRegistrationDocumentUpload a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationDocumentUpload a5 a4 a2 a1

postDriverRegistrationRegisterDl :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.RegisterDLReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationRegisterDl a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationRegisterDl a5 a4 a2 a1

postDriverRegistrationVerifyBankAccount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.VerifyBankAccountReq -> Environment.FlowHandler Kernel.External.Verification.Interface.Types.VerifyAsyncResp)
postDriverRegistrationVerifyBankAccount a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationVerifyBankAccount a5 a4 a2 a1

getDriverRegistrationInfoBankAccount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.External.Verification.Types.BankAccountVerificationResponse)
getDriverRegistrationInfoBankAccount a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationInfoBankAccount a5 a4 a2 a1

getDriverRegistrationPayoutRegistration :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.PayoutRegistrationRes)
getDriverRegistrationPayoutRegistration a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationPayoutRegistration a4 a3 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2))

postDriverRegistrationDeleteBankAccount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationDeleteBankAccount a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationDeleteBankAccount a4 a3 a1

getDriverRegistrationPayoutOrderStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.External.Payout.Interface.Types.PayoutOrderStatusResp)
getDriverRegistrationPayoutOrderStatus a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationPayoutOrderStatus a5 a4 a2 a1

postDriverRegistrationRegisterRc :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.RegisterRCReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationRegisterRc a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationRegisterRc a5 a4 a2 a1

postDriverRegistrationRegisterAadhaar :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.AadhaarCardReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationRegisterAadhaar a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationRegisterAadhaar a5 a4 a2 a1

postDriverRegistrationRegisterGenerateAadhaarOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.GenerateAadhaarOtpReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.GenerateAadhaarOtpRes)
postDriverRegistrationRegisterGenerateAadhaarOtp a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationRegisterGenerateAadhaarOtp a5 a4 a2 a1

postDriverRegistrationRegisterVerifyAadhaarOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.VerifyAadhaarOtpReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.VerifyAadhaarOtpRes)
postDriverRegistrationRegisterVerifyAadhaarOtp a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationRegisterVerifyAadhaarOtp a5 a4 a2 a1

getDriverRegistrationUnderReviewDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.UnderReviewDriversListResponse)
getDriverRegistrationUnderReviewDrivers a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationUnderReviewDrivers a5 a4 a2 a1

getDriverRegistrationDocumentsInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.StatusRes)
getDriverRegistrationDocumentsInfo a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationDocumentsInfo a4 a3 a1

getDriverRegistrationVerificationStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> API.Types.ProviderPlatform.Management.DriverRegistration.DocumentType -> API.Types.ProviderPlatform.Management.DriverRegistration.ServiceType -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.VerificationStatusListResponse)
getDriverRegistrationVerificationStatus a10 a9 _a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationVerificationStatus a10 a9 a7 a6 a5 a4 a3 a2 a1

getDriverRegistrationDocumentsCommonList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe ([API.Types.ProviderPlatform.Management.DriverRegistration.DocumentType]) -> Kernel.Prelude.Maybe ([Dashboard.Common.VerificationStatus]) -> Kernel.Prelude.Maybe ([Kernel.Types.Id.Id Dashboard.Common.Driver]) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.CommonDocumentsListRes)
getDriverRegistrationDocumentsCommonList a12 a11 _a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationDocumentsCommonList a12 a11 a9 a8 a7 a6 a5 a4 a3 a2 a1

postDriverRegistrationDocumentsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.DriverRegistration.UpdateDocumentRequest -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.UpdateDocumentResp)
postDriverRegistrationDocumentsUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ do
  res <- Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationDocumentsUpdate a4 a3 a1
  Kernel.Prelude.whenJust res.personId $ \personId -> Tools.Auth.DashboardUserAuth.updateDashboardPersonVerified personId.getId res.enabled
  Kernel.Prelude.pure res

postDriverRegistrationDocumentsCommon :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.CommonDocumentCreateReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.CommonDocumentCreateRes)
postDriverRegistrationDocumentsCommon a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationDocumentsCommon a5 a4 a2 a1

postDriverRegistrationDocumentRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.DocumentRegisterReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationDocumentRegister a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationDocumentRegister a5 a4 a2 a1

postDriverRegistrationUnlinkDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.DocumentType -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationUnlinkDocument a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  res <- Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationUnlinkDocument a5 a4 a2 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorIdForDriver a3 a2.getId)
  Kernel.Prelude.when res.mandatoryDocumentRemoved $
    Tools.Auth.DashboardUserAuth.updateDashboardPersonVerified a2.getId Kernel.Prelude.False
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

postDriverRegistrationTriggerReminder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.TriggerReminderReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationTriggerReminder a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationTriggerReminder a5 a4 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorIdForDriver a3 a2.getId) a1
