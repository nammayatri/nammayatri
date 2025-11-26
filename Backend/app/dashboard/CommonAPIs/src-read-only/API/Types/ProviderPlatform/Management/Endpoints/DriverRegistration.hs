{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration where

import qualified API.Types.ProviderPlatform.Management.Endpoints.Account
import qualified Dashboard.Common
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Servant
import Servant.Client

data AadhaarCardReq = AadhaarCardReq
  { aadhaarBackImageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Image),
    aadhaarFrontImageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Image),
    address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    consent :: Kernel.Prelude.Bool,
    consentTimestamp :: Kernel.Prelude.UTCTime,
    dateOfBirth :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    maskedAadhaarNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    nameOnCard :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    transactionId :: Kernel.Prelude.Text,
    validationStatus :: ValidationStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets AadhaarCardReq where
  hideSecrets = Kernel.Prelude.identity

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
  | Pan PanApproveDetails
  | NOC NOCApproveDetails
  | BusinessLicenseImg BusinessLicenseApproveDetails
  | CommonDocument CommonDocumentApproveDetails
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BusinessLicenseApproveDetails = BusinessLicenseApproveDetails {documentImageId :: Kernel.Types.Id.Id Dashboard.Common.Image, businessLicenseNumber :: Kernel.Prelude.Text, licenseExpiry :: Kernel.Prelude.UTCTime}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CommonDocumentApproveDetails = CommonDocumentApproveDetails {documentId :: Kernel.Types.Id.Id Dashboard.Common.CommonDriverOnboardingDocuments, updatedDocumentData :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CommonDocumentCreateReq = CommonDocumentCreateReq {documentType :: DocumentType, documentData :: Kernel.Prelude.Text, imageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Image)}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CommonDocumentCreateReq where
  hideSecrets = Kernel.Prelude.identity

data CommonDocumentItem = CommonDocumentItem
  { documentId :: Kernel.Types.Id.Id Dashboard.Common.CommonDriverOnboardingDocuments,
    documentType :: DocumentType,
    documentData :: Kernel.Prelude.Text,
    verificationStatus :: Dashboard.Common.VerificationStatus,
    rejectReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CommonDocumentRejectDetails = CommonDocumentRejectDetails {reason :: Kernel.Prelude.Text, documentId :: Kernel.Types.Id.Id Dashboard.Common.CommonDriverOnboardingDocuments}
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
  { driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverLicenseNumber :: Kernel.Prelude.Text,
    operatingCity :: Kernel.Prelude.Text,
    driverDateOfBirth :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    classOfVehicles :: [Kernel.Prelude.Text],
    imageId1 :: Kernel.Prelude.Text,
    imageId2 :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
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
  | VehicleNOC
  | Permissions
  | SubscriptionPlan
  | AadhaarCard
  | ProfileDetails
  | SocialSecurityNumber
  | GSTCertificate
  | BackgroundVerification
  | UploadProfileImage
  | BusinessLicense
  | VehicleFront
  | VehicleBack
  | VehicleRight
  | VehicleLeft
  | VehicleFrontInterior
  | VehicleBackInterior
  | Odometer
  | InspectionHub
  | KIWADriverCard
  | KIWATaxiPermit
  | KvKChamberOfCommerceRegistration
  | TAXDetails
  | BankingDetails
  | VehicleDetails
  | SchipolAirportAgreement
  | SchipolSmartcardProof
  | TXQualityMark
  | TaxiDriverPermit
  | TaxiTransportLicense
  | FinnishIDResidencePermit
  | BusinessRegistrationExtract
  | PersonalId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

instance Kernel.Types.HideSecrets.HideSecrets DocumentType where
  hideSecrets = Kernel.Prelude.identity

data DocumentsListResponse = DocumentsListResponse
  { driverLicense :: [[Kernel.Prelude.Text]],
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
    vehiclePUC :: [Kernel.Prelude.Text],
    vehicleFront :: [Kernel.Prelude.Text],
    vehicleBack :: [Kernel.Prelude.Text],
    vehicleRight :: [Kernel.Prelude.Text],
    vehicleLeft :: [Kernel.Prelude.Text],
    vehicleFrontInterior :: [Kernel.Prelude.Text],
    vehicleBackInterior :: [Kernel.Prelude.Text],
    pan :: [Kernel.Prelude.Text],
    vehicleNOC :: [Kernel.Prelude.Text],
    businessLicense :: [Kernel.Prelude.Text],
    odometer :: [Kernel.Prelude.Text],
    aadhaar :: [Kernel.Prelude.Text],
    gstCertificate :: [Kernel.Prelude.Text],
    commonDocuments :: [CommonDocumentItem]
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

data NOCApproveDetails = NOCApproveDetails {documentImageId :: Kernel.Types.Id.Id Dashboard.Common.Image, nocNumber :: Kernel.Prelude.Text, nocExpiry :: Kernel.Prelude.UTCTime, rcNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PanApproveDetails = PanApproveDetails
  { documentImageId :: Kernel.Types.Id.Id Dashboard.Common.Image,
    panNumber :: Kernel.Prelude.Text,
    docType :: Kernel.Prelude.Maybe PanType,
    driverDob :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverNameOnGovtDB :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PanType
  = INDIVIDUAL
  | BUSINESS
  deriving stock (Eq, Show, Generic)
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
    createdAt :: Kernel.Prelude.UTCTime,
    failedRules :: [Kernel.Prelude.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RegisterDLReq = RegisterDLReq
  { driverLicenseNumber :: Kernel.Prelude.Text,
    operatingCity :: Kernel.Prelude.Text,
    driverDateOfBirth :: Kernel.Prelude.UTCTime,
    imageId1 :: Kernel.Types.Id.Id Dashboard.Common.Image,
    imageId2 :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Image),
    dateOfIssue :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    accessType :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Endpoints.Account.DashboardAccessType
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
  | CommonDocumentReject CommonDocumentRejectDetails
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SSNRejectDetails = SSNRejectDetails {reason :: Kernel.Prelude.Text, ssn :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ServiceType
  = HyperVerge
  | Idfy
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

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

newtype UnlinkDocumentResp = UnlinkDocumentResp {mandatoryDocumentRemoved :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateDocumentRequest
  = Approve ApproveDetails
  | Reject RejectDetails
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateDocumentRequest where
  hideSecrets = Kernel.Prelude.identity

data UploadDocumentReq = UploadDocumentReq {imageBase64 :: Kernel.Prelude.Text, imageType :: DocumentType, rcNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text, requestorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
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

data ValidationStatus
  = APPROVED
  | DECLINED
  | AUTO_APPROVED
  | AUTO_DECLINED
  | NEEDS_REVIEW
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VerificationStatusItem = VerificationStatusItem
  { documentType :: DocumentType,
    status :: Kernel.Prelude.Text,
    verificationMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    retryCount :: Kernel.Prelude.Int,
    requestId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VerificationStatusListResponse = VerificationStatusListResponse {verificationStatuses :: [VerificationStatusItem]}
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

type API = (GetDriverRegistrationDocumentsList :<|> GetDriverRegistrationGetDocument :<|> PostDriverRegistrationDocumentUpload :<|> PostDriverRegistrationRegisterDl :<|> PostDriverRegistrationRegisterRc :<|> PostDriverRegistrationRegisterAadhaar :<|> PostDriverRegistrationRegisterGenerateAadhaarOtp :<|> PostDriverRegistrationRegisterVerifyAadhaarOtp :<|> GetDriverRegistrationUnderReviewDrivers :<|> GetDriverRegistrationDocumentsInfo :<|> GetDriverRegistrationVerificationStatus :<|> PostDriverRegistrationDocumentsUpdate :<|> PostDriverRegistrationDocumentsCommon :<|> PostDriverRegistrationUnlinkDocumentHelper)

type GetDriverRegistrationDocumentsList =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "documents" :> "list" :> QueryParam "rcId" Kernel.Prelude.Text
      :> Get
           '[JSON]
           DocumentsListResponse
  )

type GetDriverRegistrationGetDocument = ("getDocument" :> Capture "imageId" (Kernel.Types.Id.Id Dashboard.Common.Image) :> Get '[JSON] GetDocumentResponse)

type PostDriverRegistrationDocumentUpload =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "document" :> "upload" :> ReqBody '[JSON] UploadDocumentReq
      :> Post
           '[JSON]
           UploadDocumentResp
  )

type PostDriverRegistrationRegisterDl =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "register" :> "dl" :> ReqBody '[JSON] RegisterDLReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverRegistrationRegisterRc =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "register" :> "rc" :> ReqBody '[JSON] RegisterRCReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverRegistrationRegisterAadhaar =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "register" :> "aadhaar" :> ReqBody '[JSON] AadhaarCardReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverRegistrationRegisterGenerateAadhaarOtp =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "register" :> "generateAadhaarOtp"
      :> ReqBody
           '[JSON]
           GenerateAadhaarOtpReq
      :> Post '[JSON] GenerateAadhaarOtpRes
  )

type PostDriverRegistrationRegisterVerifyAadhaarOtp =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "register" :> "verifyAadhaarOtp"
      :> ReqBody
           '[JSON]
           VerifyAadhaarOtpReq
      :> Post '[JSON] VerifyAadhaarOtpRes
  )

type GetDriverRegistrationUnderReviewDrivers = ("underReviewDrivers" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> Get '[JSON] UnderReviewDriversListResponse)

type GetDriverRegistrationDocumentsInfo = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "documents" :> "info" :> Get '[JSON] [DriverDocument])

type GetDriverRegistrationVerificationStatus =
  ( "verificationStatus" :> MandatoryQueryParam "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> MandatoryQueryParam
           "fromDate"
           Kernel.Prelude.UTCTime
      :> MandatoryQueryParam "toDate" Kernel.Prelude.UTCTime
      :> MandatoryQueryParam
           "limit"
           Kernel.Prelude.Int
      :> MandatoryQueryParam
           "offset"
           Kernel.Prelude.Int
      :> MandatoryQueryParam
           "documentType"
           DocumentType
      :> MandatoryQueryParam
           "serviceType"
           ServiceType
      :> Get
           '[JSON]
           VerificationStatusListResponse
  )

type PostDriverRegistrationDocumentsUpdate = ("documents" :> "update" :> ReqBody '[JSON] UpdateDocumentRequest :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverRegistrationDocumentsCommon =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "documents" :> "common" :> ReqBody '[JSON] CommonDocumentCreateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverRegistrationUnlinkDocument =
  ( Capture "personId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "unlink" :> "document" :> Capture "documentType" DocumentType
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverRegistrationUnlinkDocumentHelper =
  ( Capture "personId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "unlink" :> "document"
      :> Capture
           "documentType"
           DocumentType
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> Post '[JSON] UnlinkDocumentResp
  )

data DriverRegistrationAPIs = DriverRegistrationAPIs
  { getDriverRegistrationDocumentsList :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient DocumentsListResponse,
    getDriverRegistrationGetDocument :: Kernel.Types.Id.Id Dashboard.Common.Image -> EulerHS.Types.EulerClient GetDocumentResponse,
    postDriverRegistrationDocumentUpload :: Kernel.Types.Id.Id Dashboard.Common.Driver -> UploadDocumentReq -> EulerHS.Types.EulerClient UploadDocumentResp,
    postDriverRegistrationRegisterDl :: Kernel.Types.Id.Id Dashboard.Common.Driver -> RegisterDLReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverRegistrationRegisterRc :: Kernel.Types.Id.Id Dashboard.Common.Driver -> RegisterRCReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverRegistrationRegisterAadhaar :: Kernel.Types.Id.Id Dashboard.Common.Driver -> AadhaarCardReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverRegistrationRegisterGenerateAadhaarOtp :: Kernel.Types.Id.Id Dashboard.Common.Driver -> GenerateAadhaarOtpReq -> EulerHS.Types.EulerClient GenerateAadhaarOtpRes,
    postDriverRegistrationRegisterVerifyAadhaarOtp :: Kernel.Types.Id.Id Dashboard.Common.Driver -> VerifyAadhaarOtpReq -> EulerHS.Types.EulerClient VerifyAadhaarOtpRes,
    getDriverRegistrationUnderReviewDrivers :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient UnderReviewDriversListResponse,
    getDriverRegistrationDocumentsInfo :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient [DriverDocument],
    getDriverRegistrationVerificationStatus :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> DocumentType -> ServiceType -> EulerHS.Types.EulerClient VerificationStatusListResponse,
    postDriverRegistrationDocumentsUpdate :: UpdateDocumentRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverRegistrationDocumentsCommon :: Kernel.Types.Id.Id Dashboard.Common.Driver -> CommonDocumentCreateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverRegistrationUnlinkDocument :: Kernel.Types.Id.Id Dashboard.Common.Driver -> DocumentType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient UnlinkDocumentResp
  }

mkDriverRegistrationAPIs :: (Client EulerHS.Types.EulerClient API -> DriverRegistrationAPIs)
mkDriverRegistrationAPIs driverRegistrationClient = (DriverRegistrationAPIs {..})
  where
    getDriverRegistrationDocumentsList :<|> getDriverRegistrationGetDocument :<|> postDriverRegistrationDocumentUpload :<|> postDriverRegistrationRegisterDl :<|> postDriverRegistrationRegisterRc :<|> postDriverRegistrationRegisterAadhaar :<|> postDriverRegistrationRegisterGenerateAadhaarOtp :<|> postDriverRegistrationRegisterVerifyAadhaarOtp :<|> getDriverRegistrationUnderReviewDrivers :<|> getDriverRegistrationDocumentsInfo :<|> getDriverRegistrationVerificationStatus :<|> postDriverRegistrationDocumentsUpdate :<|> postDriverRegistrationDocumentsCommon :<|> postDriverRegistrationUnlinkDocument = driverRegistrationClient

data DriverRegistrationUserActionType
  = GET_DRIVER_REGISTRATION_DOCUMENTS_LIST
  | GET_DRIVER_REGISTRATION_GET_DOCUMENT
  | POST_DRIVER_REGISTRATION_DOCUMENT_UPLOAD
  | POST_DRIVER_REGISTRATION_REGISTER_DL
  | POST_DRIVER_REGISTRATION_REGISTER_RC
  | POST_DRIVER_REGISTRATION_REGISTER_AADHAAR
  | POST_DRIVER_REGISTRATION_REGISTER_GENERATE_AADHAAR_OTP
  | POST_DRIVER_REGISTRATION_REGISTER_VERIFY_AADHAAR_OTP
  | GET_DRIVER_REGISTRATION_UNDER_REVIEW_DRIVERS
  | GET_DRIVER_REGISTRATION_DOCUMENTS_INFO
  | GET_DRIVER_REGISTRATION_VERIFICATION_STATUS
  | POST_DRIVER_REGISTRATION_DOCUMENTS_UPDATE
  | POST_DRIVER_REGISTRATION_DOCUMENTS_COMMON
  | POST_DRIVER_REGISTRATION_UNLINK_DOCUMENT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''DocumentType)

$(mkHttpInstancesForEnum ''ServiceType)

$(Data.Singletons.TH.genSingletons [''DriverRegistrationUserActionType])
