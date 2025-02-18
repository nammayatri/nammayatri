{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.DriverRegistration
  ( API.Types.ProviderPlatform.Management.DriverRegistration.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.DriverRegistration
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.DriverRegistration as Domain.Action.Dashboard.Management.DriverRegistration
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.DriverRegistration.API)
handler merchantId city = getDriverRegistrationDocumentsList merchantId city :<|> getDriverRegistrationGetDocument merchantId city :<|> postDriverRegistrationDocumentUpload merchantId city :<|> postDriverRegistrationRegisterDl merchantId city :<|> postDriverRegistrationRegisterRc merchantId city :<|> postDriverRegistrationRegisterGenerateAadhaarOtp merchantId city :<|> postDriverRegistrationRegisterVerifyAadhaarOtp merchantId city :<|> getDriverRegistrationUnderReviewDrivers merchantId city :<|> getDriverRegistrationDocumentsInfo merchantId city :<|> postDriverRegistrationDocumentsUpdate merchantId city

getDriverRegistrationDocumentsList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.DocumentsListResponse)
getDriverRegistrationDocumentsList a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationDocumentsList a3 a2 a1

getDriverRegistrationGetDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Image -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.GetDocumentResponse)
getDriverRegistrationGetDocument a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationGetDocument a3 a2 a1

postDriverRegistrationDocumentUpload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.UploadDocumentReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.UploadDocumentResp)
postDriverRegistrationDocumentUpload a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationDocumentUpload a4 a3 a2 a1

postDriverRegistrationRegisterDl :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.RegisterDLReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationRegisterDl a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationRegisterDl a4 a3 a2 a1

postDriverRegistrationRegisterRc :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.RegisterRCReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationRegisterRc a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationRegisterRc a4 a3 a2 a1

postDriverRegistrationRegisterGenerateAadhaarOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.GenerateAadhaarOtpReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.GenerateAadhaarOtpRes)
postDriverRegistrationRegisterGenerateAadhaarOtp a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationRegisterGenerateAadhaarOtp a4 a3 a2 a1

postDriverRegistrationRegisterVerifyAadhaarOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.VerifyAadhaarOtpReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.VerifyAadhaarOtpRes)
postDriverRegistrationRegisterVerifyAadhaarOtp a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationRegisterVerifyAadhaarOtp a4 a3 a2 a1

getDriverRegistrationUnderReviewDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.UnderReviewDriversListResponse)
getDriverRegistrationUnderReviewDrivers a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationUnderReviewDrivers a4 a3 a2 a1

getDriverRegistrationDocumentsInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.DriverRegistration.DriverDocument])
getDriverRegistrationDocumentsInfo a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.getDriverRegistrationDocumentsInfo a3 a2 a1

postDriverRegistrationDocumentsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.DriverRegistration.UpdateDocumentRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationDocumentsUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverRegistration.postDriverRegistrationDocumentsUpdate a3 a2 a1
