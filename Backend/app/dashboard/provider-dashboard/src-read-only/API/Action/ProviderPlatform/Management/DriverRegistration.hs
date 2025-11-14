{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.DriverRegistration
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.DriverRegistration
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.DriverRegistration
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("driver" :> (GetDriverRegistrationDocumentsList :<|> GetDriverRegistrationGetDocument :<|> PostDriverRegistrationDocumentUpload :<|> PostDriverRegistrationRegisterDl :<|> PostDriverRegistrationRegisterRc :<|> PostDriverRegistrationRegisterAadhaar :<|> PostDriverRegistrationRegisterGenerateAadhaarOtp :<|> PostDriverRegistrationRegisterVerifyAadhaarOtp :<|> GetDriverRegistrationUnderReviewDrivers :<|> GetDriverRegistrationDocumentsInfo :<|> GetDriverRegistrationVerificationStatus :<|> PostDriverRegistrationDocumentsUpdate :<|> PostDriverRegistrationDocumentsCommon :<|> PostDriverRegistrationUnlinkDocument))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverRegistrationDocumentsList merchantId city :<|> getDriverRegistrationGetDocument merchantId city :<|> postDriverRegistrationDocumentUpload merchantId city :<|> postDriverRegistrationRegisterDl merchantId city :<|> postDriverRegistrationRegisterRc merchantId city :<|> postDriverRegistrationRegisterAadhaar merchantId city :<|> postDriverRegistrationRegisterGenerateAadhaarOtp merchantId city :<|> postDriverRegistrationRegisterVerifyAadhaarOtp merchantId city :<|> getDriverRegistrationUnderReviewDrivers merchantId city :<|> getDriverRegistrationDocumentsInfo merchantId city :<|> getDriverRegistrationVerificationStatus merchantId city :<|> postDriverRegistrationDocumentsUpdate merchantId city :<|> postDriverRegistrationDocumentsCommon merchantId city :<|> postDriverRegistrationUnlinkDocument merchantId city

type GetDriverRegistrationDocumentsList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_REGISTRATION / 'API.Types.ProviderPlatform.Management.DriverRegistration.GET_DRIVER_REGISTRATION_DOCUMENTS_LIST)
      :> API.Types.ProviderPlatform.Management.DriverRegistration.GetDriverRegistrationDocumentsList
  )

type GetDriverRegistrationGetDocument =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_REGISTRATION / 'API.Types.ProviderPlatform.Management.DriverRegistration.GET_DRIVER_REGISTRATION_GET_DOCUMENT)
      :> API.Types.ProviderPlatform.Management.DriverRegistration.GetDriverRegistrationGetDocument
  )

type PostDriverRegistrationDocumentUpload =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_REGISTRATION / 'API.Types.ProviderPlatform.Management.DriverRegistration.POST_DRIVER_REGISTRATION_DOCUMENT_UPLOAD)
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationDocumentUpload
  )

type PostDriverRegistrationRegisterDl =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_REGISTRATION / 'API.Types.ProviderPlatform.Management.DriverRegistration.POST_DRIVER_REGISTRATION_REGISTER_DL)
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationRegisterDl
  )

type PostDriverRegistrationRegisterRc =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_REGISTRATION / 'API.Types.ProviderPlatform.Management.DriverRegistration.POST_DRIVER_REGISTRATION_REGISTER_RC)
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationRegisterRc
  )

type PostDriverRegistrationRegisterAadhaar =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_REGISTRATION / 'API.Types.ProviderPlatform.Management.DriverRegistration.POST_DRIVER_REGISTRATION_REGISTER_AADHAAR)
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationRegisterAadhaar
  )

type PostDriverRegistrationRegisterGenerateAadhaarOtp =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_REGISTRATION / 'API.Types.ProviderPlatform.Management.DriverRegistration.POST_DRIVER_REGISTRATION_REGISTER_GENERATE_AADHAAR_OTP)
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationRegisterGenerateAadhaarOtp
  )

type PostDriverRegistrationRegisterVerifyAadhaarOtp =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_REGISTRATION / 'API.Types.ProviderPlatform.Management.DriverRegistration.POST_DRIVER_REGISTRATION_REGISTER_VERIFY_AADHAAR_OTP)
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationRegisterVerifyAadhaarOtp
  )

type GetDriverRegistrationUnderReviewDrivers =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_REGISTRATION / 'API.Types.ProviderPlatform.Management.DriverRegistration.GET_DRIVER_REGISTRATION_UNDER_REVIEW_DRIVERS)
      :> API.Types.ProviderPlatform.Management.DriverRegistration.GetDriverRegistrationUnderReviewDrivers
  )

type GetDriverRegistrationDocumentsInfo =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_REGISTRATION / 'API.Types.ProviderPlatform.Management.DriverRegistration.GET_DRIVER_REGISTRATION_DOCUMENTS_INFO)
      :> API.Types.ProviderPlatform.Management.DriverRegistration.GetDriverRegistrationDocumentsInfo
  )

type GetDriverRegistrationVerificationStatus =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_REGISTRATION / 'API.Types.ProviderPlatform.Management.DriverRegistration.GET_DRIVER_REGISTRATION_VERIFICATION_STATUS)
      :> API.Types.ProviderPlatform.Management.DriverRegistration.GetDriverRegistrationVerificationStatus
  )

type PostDriverRegistrationDocumentsUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_REGISTRATION / 'API.Types.ProviderPlatform.Management.DriverRegistration.POST_DRIVER_REGISTRATION_DOCUMENTS_UPDATE)
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationDocumentsUpdate
  )

type PostDriverRegistrationDocumentsCommon =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_REGISTRATION / 'API.Types.ProviderPlatform.Management.DriverRegistration.POST_DRIVER_REGISTRATION_DOCUMENTS_COMMON)
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationDocumentsCommon
  )

type PostDriverRegistrationUnlinkDocument =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_REGISTRATION / 'API.Types.ProviderPlatform.Management.DriverRegistration.POST_DRIVER_REGISTRATION_UNLINK_DOCUMENT)
      :> API.Types.ProviderPlatform.Management.DriverRegistration.PostDriverRegistrationUnlinkDocument
  )

getDriverRegistrationDocumentsList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.DocumentsListResponse)
getDriverRegistrationDocumentsList merchantShortId opCity apiTokenInfo driverId rcId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverRegistration.getDriverRegistrationDocumentsList merchantShortId opCity apiTokenInfo driverId rcId

getDriverRegistrationGetDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Image -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.GetDocumentResponse)
getDriverRegistrationGetDocument merchantShortId opCity apiTokenInfo imageId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverRegistration.getDriverRegistrationGetDocument merchantShortId opCity apiTokenInfo imageId

postDriverRegistrationDocumentUpload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.UploadDocumentReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.UploadDocumentResp)
postDriverRegistrationDocumentUpload merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverRegistration.postDriverRegistrationDocumentUpload merchantShortId opCity apiTokenInfo driverId req

postDriverRegistrationRegisterDl :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.RegisterDLReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationRegisterDl merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverRegistration.postDriverRegistrationRegisterDl merchantShortId opCity apiTokenInfo driverId req

postDriverRegistrationRegisterRc :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.RegisterRCReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationRegisterRc merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverRegistration.postDriverRegistrationRegisterRc merchantShortId opCity apiTokenInfo driverId req

postDriverRegistrationRegisterAadhaar :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.AadhaarCardReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationRegisterAadhaar merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverRegistration.postDriverRegistrationRegisterAadhaar merchantShortId opCity apiTokenInfo driverId req

postDriverRegistrationRegisterGenerateAadhaarOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.GenerateAadhaarOtpReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.GenerateAadhaarOtpRes)
postDriverRegistrationRegisterGenerateAadhaarOtp merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverRegistration.postDriverRegistrationRegisterGenerateAadhaarOtp merchantShortId opCity apiTokenInfo driverId req

postDriverRegistrationRegisterVerifyAadhaarOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.VerifyAadhaarOtpReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.VerifyAadhaarOtpRes)
postDriverRegistrationRegisterVerifyAadhaarOtp merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverRegistration.postDriverRegistrationRegisterVerifyAadhaarOtp merchantShortId opCity apiTokenInfo driverId req

getDriverRegistrationUnderReviewDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.UnderReviewDriversListResponse)
getDriverRegistrationUnderReviewDrivers merchantShortId opCity apiTokenInfo limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverRegistration.getDriverRegistrationUnderReviewDrivers merchantShortId opCity apiTokenInfo limit offset

getDriverRegistrationDocumentsInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.DriverRegistration.DriverDocument])
getDriverRegistrationDocumentsInfo merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverRegistration.getDriverRegistrationDocumentsInfo merchantShortId opCity apiTokenInfo driverId

getDriverRegistrationVerificationStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> API.Types.ProviderPlatform.Management.DriverRegistration.DocumentType -> API.Types.ProviderPlatform.Management.DriverRegistration.ServiceType -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverRegistration.VerificationStatusListResponse)
getDriverRegistrationVerificationStatus merchantShortId opCity apiTokenInfo driverId fromDate toDate limit offset documentType serviceType = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverRegistration.getDriverRegistrationVerificationStatus merchantShortId opCity apiTokenInfo driverId fromDate toDate limit offset documentType serviceType

postDriverRegistrationDocumentsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.DriverRegistration.UpdateDocumentRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationDocumentsUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverRegistration.postDriverRegistrationDocumentsUpdate merchantShortId opCity apiTokenInfo req

postDriverRegistrationDocumentsCommon :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.CommonDocumentCreateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationDocumentsCommon merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverRegistration.postDriverRegistrationDocumentsCommon merchantShortId opCity apiTokenInfo driverId req

postDriverRegistrationUnlinkDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverRegistration.DocumentType -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationUnlinkDocument merchantShortId opCity apiTokenInfo personId documentType = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverRegistration.postDriverRegistrationUnlinkDocument merchantShortId opCity apiTokenInfo personId documentType
