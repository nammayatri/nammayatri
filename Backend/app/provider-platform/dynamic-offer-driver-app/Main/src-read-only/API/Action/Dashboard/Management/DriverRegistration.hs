{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.DriverRegistration
  ( API.Types.ProviderPlatform.Management.DriverRegistration.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.DriverRegistration
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.DriverRegistration
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Verification.Interface.Types
import qualified Kernel.External.Verification.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.DriverRegistration.API)
handler merchantId city = getDriverRegistrationDocumentsList merchantId city :<|> getDriverRegistrationGetDocument merchantId city :<|> postDriverRegistrationDocumentUpload merchantId city :<|> postDriverRegistrationRegisterDl merchantId city :<|> postDriverRegistrationVerifyBankAccount merchantId city :<|> getDriverRegistrationInfoBankAccount merchantId city :<|> postDriverRegistrationRegisterRc merchantId city :<|> postDriverRegistrationRegisterAadhaar merchantId city :<|> postDriverRegistrationRegisterGenerateAadhaarOtp merchantId city :<|> postDriverRegistrationRegisterVerifyAadhaarOtp merchantId city :<|> getDriverRegistrationUnderReviewDrivers merchantId city :<|> getDriverRegistrationDocumentsInfo merchantId city :<|> getDriverRegistrationVerificationStatus merchantId city :<|> postDriverRegistrationDocumentsUpdate merchantId city :<|> postDriverRegistrationDocumentsCommon merchantId city :<|> postDriverRegistrationUnlinkDocument merchantId city :<|> postDriverRegistrationTriggerReminder merchantId city

getDriverRegistrationDocumentsList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.DocumentsListResponse)
getDriverRegistrationDocumentsList a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationDocumentsList a4 a3 a2 a1

getDriverRegistrationGetDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Image -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.GetDocumentResponse)
getDriverRegistrationGetDocument a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationGetDocument a3 a2 a1

postDriverRegistrationDocumentUpload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.UploadDocumentReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.UploadDocumentResp)
postDriverRegistrationDocumentUpload a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationDocumentUpload a4 a3 a2 a1

postDriverRegistrationRegisterDl :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.RegisterDLReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationRegisterDl a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationRegisterDl a4 a3 a2 a1

postDriverRegistrationVerifyBankAccount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.VerifyBankAccountReq -> Environment.FlowHandler Kernel.External.Verification.Interface.Types.VerifyAsyncResp)
postDriverRegistrationVerifyBankAccount a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationVerifyBankAccount a4 a3 a2 a1

getDriverRegistrationInfoBankAccount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.External.Verification.Types.BankAccountVerificationResponse)
getDriverRegistrationInfoBankAccount a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationInfoBankAccount a4 a3 a2 a1

postDriverRegistrationRegisterRc :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.RegisterRCReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationRegisterRc a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationRegisterRc a4 a3 a2 a1

postDriverRegistrationRegisterAadhaar :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.AadhaarCardReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationRegisterAadhaar a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationRegisterAadhaar a4 a3 a2 a1

postDriverRegistrationRegisterGenerateAadhaarOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.GenerateAadhaarOtpReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.GenerateAadhaarOtpRes)
postDriverRegistrationRegisterGenerateAadhaarOtp a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationRegisterGenerateAadhaarOtp a4 a3 a2 a1

postDriverRegistrationRegisterVerifyAadhaarOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.VerifyAadhaarOtpReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.VerifyAadhaarOtpRes)
postDriverRegistrationRegisterVerifyAadhaarOtp a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationRegisterVerifyAadhaarOtp a4 a3 a2 a1

getDriverRegistrationUnderReviewDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.UnderReviewDriversListResponse)
getDriverRegistrationUnderReviewDrivers a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationUnderReviewDrivers a4 a3 a2 a1

getDriverRegistrationDocumentsInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.DriverRegistration.DriverDocument])
getDriverRegistrationDocumentsInfo a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationDocumentsInfo a3 a2 a1

getDriverRegistrationVerificationStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> API.Types.ProviderPlatform.Management.DriverRegistration.DocumentType -> API.Types.ProviderPlatform.Management.DriverRegistration.ServiceType -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.VerificationStatusListResponse)
getDriverRegistrationVerificationStatus a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationVerificationStatus a9 a8 a7 a6 a5 a4 a3 a2 a1

postDriverRegistrationDocumentsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.DriverRegistration.UpdateDocumentRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationDocumentsUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationDocumentsUpdate a3 a2 a1

postDriverRegistrationDocumentsCommon :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.CommonDocumentCreateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationDocumentsCommon a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationDocumentsCommon a4 a3 a2 a1

postDriverRegistrationUnlinkDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.DocumentType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.UnlinkDocumentResp)
postDriverRegistrationUnlinkDocument a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationUnlinkDocument a5 a4 a3 a2 a1

postDriverRegistrationTriggerReminder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.DriverRegistration.TriggerReminderReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationTriggerReminder a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationTriggerReminder a5 a4 a3 a2 a1
