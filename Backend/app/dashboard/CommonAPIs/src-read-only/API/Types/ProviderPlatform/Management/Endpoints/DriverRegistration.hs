{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data ApproveDetails
  = RC RCApproveDetails
  | DL DLApproveDetails
  | UploadProfile (Kernel.Types.Id.Id Dashboard.Common.Image)
  | SSNApprove Kernel.Prelude.Text
  | ProfilePhoto (Kernel.Types.Id.Id Dashboard.Common.Image)
  | VehiclePermit VPermitApproveDetails
  | VehiclePUC VPUCApproveDetails
  | VehicleInsurance VInsuranceApproveDetails
  | VehicleFitnessCertificate FitnessApproveDetails
  | VehicleInspectionForm VInspectionApproveDetails
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DLApproveDetails = DLApproveDetails
  { documentImageId :: Kernel.Types.Id.Id Dashboard.Common.Image,
    driverLicenseNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverDateOfBirth :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    dateOfExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DLDetails = DLDetails
  { driverLicenseNumber :: Kernel.Prelude.Text,
    operatingCity :: Kernel.Prelude.Text,
    driverDateOfBirth :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    classOfVehicles :: [Kernel.Prelude.Text],
    imageId1 :: Kernel.Prelude.Text,
    imageId2 :: Kernel.Prelude.Maybe (Kernel.Prelude.Text),
    dateOfIssue :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype DocumentInfo = DocumentInfo {documentNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentType
  = DriverLicense
  | VehicleRegistrationCertificate
  | PanCard
  | VehiclePermitImage
  | VehiclePUCImage
  | VehicleInsuranceImage
  | VehicleFitnessCertificateImage
  | VehicleInspectionImage
  | ProfilePhotoImage
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentsListResponse = DocumentsListResponse
  { driverLicense :: [Kernel.Prelude.Text],
    vehicleRegistrationCertificate :: [Kernel.Prelude.Text],
    driverLicenseDetails :: [DLDetails],
    vehicleRegistrationCertificateDetails :: [RCDetails],
    vehicleInsurance :: [Kernel.Prelude.Text],
    uploadProfile :: [Kernel.Prelude.Text],
    ssn :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleFitnessCertificate :: [Kernel.Prelude.Text],
    profilePhoto :: [Kernel.Prelude.Text],
    vehicleInspectionForm :: [Kernel.Prelude.Text],
    vehiclePermit :: [Kernel.Prelude.Text],
    vehiclePUC :: [Kernel.Prelude.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverDocument = DriverDocument
  { documentType :: DocumentType,
    documentStatus :: Dashboard.Common.VerificationStatus,
    documentImageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Image),
    documentImages :: [Kernel.Types.Id.Id Dashboard.Common.Image],
    documentId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Document),
    documentInfo :: Kernel.Prelude.Maybe DocumentInfo
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FitnessApproveDetails = FitnessApproveDetails
  { applicationNumber :: Kernel.Prelude.Text,
    categoryOfVehicle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentImageId :: Kernel.Types.Id.Id Dashboard.Common.Image,
    fitnessExpiry :: Kernel.Prelude.UTCTime,
    inspectingAuthority :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    inspectingOn :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    nextInspectionDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rcNumber :: Kernel.Prelude.Text,
    receiptDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GenerateAadhaarOtpReq = GenerateAadhaarOtpReq {aadhaarNumber :: Kernel.Prelude.Text, consent :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets GenerateAadhaarOtpReq where
  hideSecrets = Kernel.Prelude.identity

data GenerateAadhaarOtpRes = GenerateAadhaarOtpRes {message :: Kernel.Prelude.Text, transactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets GenerateAadhaarOtpRes where
  hideSecrets = Kernel.Prelude.identity

data GetDocumentResponse = GetDocumentResponse {imageBase64 :: Kernel.Prelude.Text, status :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ImageDocumentsRejectDetails = ImageDocumentsRejectDetails {reason :: Kernel.Prelude.Text, documentImageId :: Kernel.Types.Id.Id Dashboard.Common.Image}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RCApproveDetails = RCApproveDetails
  { documentImageId :: Kernel.Types.Id.Id Dashboard.Common.Image,
    vehicleVariant :: Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant,
    vehicleNumberPlate :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleManufacturer :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleModelYear :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleDoors :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleSeatBelts :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    fitnessExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    permitExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RCDetails = RCDetails
  { vehicleRegistrationCertNumber :: Kernel.Prelude.Text,
    imageId :: Kernel.Prelude.Text,
    operatingCity :: Kernel.Prelude.Text,
    dateOfRegistration :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    airConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    vehicleManufacturer :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleDoors :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleSeatBelts :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleModelYear :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    oxygen :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    ventilator :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RegisterDLReq = RegisterDLReq
  { driverLicenseNumber :: Kernel.Prelude.Text,
    operatingCity :: Kernel.Prelude.Text,
    driverDateOfBirth :: Kernel.Prelude.UTCTime,
    imageId1 :: Kernel.Types.Id.Id Dashboard.Common.Image,
    imageId2 :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Image),
    dateOfIssue :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RegisterDLReq where
  hideSecrets = Kernel.Prelude.identity

data RegisterRCReq = RegisterRCReq
  { vehicleRegistrationCertNumber :: Kernel.Prelude.Text,
    imageId :: Kernel.Types.Id.Id Dashboard.Common.Image,
    operatingCity :: Kernel.Prelude.Text,
    dateOfRegistration :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    airConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    multipleRC :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    oxygen :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    ventilator :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    vehicleCategory :: Kernel.Prelude.Maybe Dashboard.Common.VehicleCategory,
    vehicleDetails :: Kernel.Prelude.Maybe Dashboard.Common.DriverVehicleDetails
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RegisterRCReq where
  hideSecrets = Kernel.Prelude.identity

data RejectDetails
  = SSNReject SSNRejectDetails
  | ImageDocuments ImageDocumentsRejectDetails
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SSNRejectDetails = SSNRejectDetails {reason :: Kernel.Prelude.Text, ssn :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UnderReviewDriver = UnderReviewDriver
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    driverName :: Kernel.Prelude.Text,
    driverMobileNumber :: Kernel.Prelude.Text,
    driverMobileCountryCode :: Kernel.Prelude.Text,
    reviewedBy :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UnderReviewDriversListResponse = UnderReviewDriversListResponse {drivers :: [UnderReviewDriver], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateDocumentRequest
  = Approve ApproveDetails
  | Reject RejectDetails
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateDocumentRequest where
  hideSecrets = Kernel.Prelude.identity

data UploadDocumentReq = UploadDocumentReq {imageBase64 :: Kernel.Prelude.Text, imageType :: DocumentType, rcNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype UploadDocumentResp = UploadDocumentResp {imageId :: Kernel.Types.Id.Id Dashboard.Common.Image}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UploadDocumentResp where
  hideSecrets = Kernel.Prelude.identity

data UploadDocumentTReq = UploadDocumentTReq {imageType :: DocumentType, rcNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VInspectionApproveDetails = VInspectionApproveDetails {documentImageId :: Kernel.Types.Id.Id Dashboard.Common.Image, dateOfExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VInsuranceApproveDetails = VInsuranceApproveDetails
  { documentImageId :: Kernel.Types.Id.Id Dashboard.Common.Image,
    insuredName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issueDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    limitsOfLiability :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    policyExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    policyNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    policyProvider :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rcNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VPUCApproveDetails = VPUCApproveDetails
  { documentImageId :: Kernel.Types.Id.Id Dashboard.Common.Image,
    pucNumber :: Kernel.Prelude.Text,
    pucExpiry :: Kernel.Prelude.UTCTime,
    rcNumber :: Kernel.Prelude.Text,
    testDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VPermitApproveDetails = VPermitApproveDetails
  { documentImageId :: Kernel.Types.Id.Id Dashboard.Common.Image,
    issueDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    nameOfPermitHolder :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    permitExpiry :: Kernel.Prelude.UTCTime,
    permitNumber :: Kernel.Prelude.Text,
    purposeOfJourney :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rcNumber :: Kernel.Prelude.Text,
    regionCovered :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VerifyAadhaarOtpReq = VerifyAadhaarOtpReq {otp :: Kernel.Prelude.Int, shareCode :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets VerifyAadhaarOtpReq where
  hideSecrets = Kernel.Prelude.identity

data VerifyAadhaarOtpRes = VerifyAadhaarOtpRes
  { message :: Kernel.Prelude.Text,
    code :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    gender :: Kernel.Prelude.Text,
    date_of_birth :: Kernel.Prelude.Text,
    share_code :: Kernel.Prelude.Text,
    image :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets VerifyAadhaarOtpRes where
  hideSecrets = Kernel.Prelude.identity

type API = (GetDriverRegistrationDocumentsList :<|> GetDriverRegistrationGetDocument :<|> PostDriverRegistrationDocumentUpload :<|> PostDriverRegistrationRegisterDl :<|> PostDriverRegistrationRegisterRc :<|> PostDriverRegistrationRegisterGenerateAadhaarOtp :<|> PostDriverRegistrationRegisterVerifyAadhaarOtp :<|> GetDriverRegistrationUnderReviewDrivers :<|> GetDriverRegistrationDocumentsInfo :<|> PostDriverRegistrationDocumentsUpdate)

type GetDriverRegistrationDocumentsList = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "documents" :> "list" :> Get ('[JSON]) DocumentsListResponse)

type GetDriverRegistrationGetDocument = ("getDocument" :> Capture "imageId" (Kernel.Types.Id.Id Dashboard.Common.Image) :> Get ('[JSON]) GetDocumentResponse)

type PostDriverRegistrationDocumentUpload =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "document" :> "upload" :> ReqBody ('[JSON]) UploadDocumentReq
      :> Post
           ('[JSON])
           UploadDocumentResp
  )

type PostDriverRegistrationRegisterDl =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "register" :> "dl" :> ReqBody ('[JSON]) RegisterDLReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverRegistrationRegisterRc =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "register" :> "rc" :> ReqBody ('[JSON]) RegisterRCReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverRegistrationRegisterGenerateAadhaarOtp =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "register" :> "generateAadhaarOtp"
      :> ReqBody
           ('[JSON])
           GenerateAadhaarOtpReq
      :> Post ('[JSON]) GenerateAadhaarOtpRes
  )

type PostDriverRegistrationRegisterVerifyAadhaarOtp =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "register" :> "verifyAadhaarOtp"
      :> ReqBody
           ('[JSON])
           VerifyAadhaarOtpReq
      :> Post ('[JSON]) VerifyAadhaarOtpRes
  )

type GetDriverRegistrationUnderReviewDrivers = ("underReviewDrivers" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> Get ('[JSON]) UnderReviewDriversListResponse)

type GetDriverRegistrationDocumentsInfo = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "documents" :> "info" :> Get ('[JSON]) [DriverDocument])

type PostDriverRegistrationDocumentsUpdate = ("documents" :> "update" :> ReqBody ('[JSON]) UpdateDocumentRequest :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data DriverRegistrationAPIs = DriverRegistrationAPIs
  { getDriverRegistrationDocumentsList :: (Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient DocumentsListResponse),
    getDriverRegistrationGetDocument :: (Kernel.Types.Id.Id Dashboard.Common.Image -> EulerHS.Types.EulerClient GetDocumentResponse),
    postDriverRegistrationDocumentUpload :: (Kernel.Types.Id.Id Dashboard.Common.Driver -> UploadDocumentReq -> EulerHS.Types.EulerClient UploadDocumentResp),
    postDriverRegistrationRegisterDl :: (Kernel.Types.Id.Id Dashboard.Common.Driver -> RegisterDLReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postDriverRegistrationRegisterRc :: (Kernel.Types.Id.Id Dashboard.Common.Driver -> RegisterRCReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postDriverRegistrationRegisterGenerateAadhaarOtp :: (Kernel.Types.Id.Id Dashboard.Common.Driver -> GenerateAadhaarOtpReq -> EulerHS.Types.EulerClient GenerateAadhaarOtpRes),
    postDriverRegistrationRegisterVerifyAadhaarOtp :: (Kernel.Types.Id.Id Dashboard.Common.Driver -> VerifyAadhaarOtpReq -> EulerHS.Types.EulerClient VerifyAadhaarOtpRes),
    getDriverRegistrationUnderReviewDrivers :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient UnderReviewDriversListResponse),
    getDriverRegistrationDocumentsInfo :: (Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient [DriverDocument]),
    postDriverRegistrationDocumentsUpdate :: (UpdateDocumentRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkDriverRegistrationAPIs :: (Client EulerHS.Types.EulerClient API -> DriverRegistrationAPIs)
mkDriverRegistrationAPIs driverRegistrationClient = (DriverRegistrationAPIs {..})
  where
    getDriverRegistrationDocumentsList :<|> getDriverRegistrationGetDocument :<|> postDriverRegistrationDocumentUpload :<|> postDriverRegistrationRegisterDl :<|> postDriverRegistrationRegisterRc :<|> postDriverRegistrationRegisterGenerateAadhaarOtp :<|> postDriverRegistrationRegisterVerifyAadhaarOtp :<|> getDriverRegistrationUnderReviewDrivers :<|> getDriverRegistrationDocumentsInfo :<|> postDriverRegistrationDocumentsUpdate = driverRegistrationClient

data DriverRegistrationUserActionType
  = GET_DRIVER_REGISTRATION_DOCUMENTS_LIST
  | GET_DRIVER_REGISTRATION_GET_DOCUMENT
  | POST_DRIVER_REGISTRATION_DOCUMENT_UPLOAD
  | POST_DRIVER_REGISTRATION_REGISTER_DL
  | POST_DRIVER_REGISTRATION_REGISTER_RC
  | POST_DRIVER_REGISTRATION_REGISTER_GENERATE_AADHAAR_OTP
  | POST_DRIVER_REGISTRATION_REGISTER_VERIFY_AADHAAR_OTP
  | GET_DRIVER_REGISTRATION_UNDER_REVIEW_DRIVERS
  | GET_DRIVER_REGISTRATION_DOCUMENTS_INFO
  | POST_DRIVER_REGISTRATION_DOCUMENTS_UPDATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''DriverRegistrationUserActionType)])
