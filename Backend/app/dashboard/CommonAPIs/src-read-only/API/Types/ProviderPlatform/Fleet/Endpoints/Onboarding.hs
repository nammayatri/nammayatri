{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.OnboardingExtra
import qualified API.Types.ProviderPlatform.Management.Endpoints.Account
import qualified API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import qualified Domain.Types.VehicleCategory
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Servant
import Servant.Client

data AadhaarDocumentMetadata = AadhaarDocumentMetadata
  { aadhaarNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dateOfBirth :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    nameOnCard :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BankingDetailsDocumentMetadata = BankingDetailsDocumentMetadata
  { accountNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ifscCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    nameAtBank :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    upiId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DLDocumentMetadata = DLDocumentMetadata {dateOfExpiry :: Kernel.Prelude.UTCTime, driverDateOfBirth :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime, driverLicenseNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentApplicableType
  = FLEET
  | INDIVIDUAL
  | FLEET_AND_INDIVIDUAL
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentCategory
  = Driver
  | Vehicle
  | Permission
  | Training
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentFlowGrouping
  = COMMON
  | STANDARD
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentMetadata
  = DLMetadata DLDocumentMetadata
  | AadhaarMetadata AadhaarDocumentMetadata
  | PanMetadata PanDocumentMetadata
  | LocalAddressProofMetadata LocalAddressProofDocumentMetadata
  | GSTMetadata GSTDocumentMetadata
  | RCMetadata RCDocumentMetadata
  | VehiclePUCMetadata VehiclePUCDocumentMetadata
  | VehicleFitnessMetadata VehicleFitnessCertificateDocumentMetadata
  | VehicleInsuranceMetadata VehicleInsuranceDocumentMetadata
  | VehiclePermitMetadata VehiclePermitDocumentMetadata
  | UDYAMMetadata UDYAMDocumentMetadata
  | TANMetadata TANDocumentMetadata
  | LDCMetadata LDCDocumentMetadata
  | NomineeDetailsMetadata NomineeDetailsDocumentMetadata
  | BankingDetailsMetadata BankingDetailsDocumentMetadata
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentStatusItem = DocumentStatusItem
  { documentType :: API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.DocumentType,
    expiryDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    imageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    imageId2 :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    metadata :: Kernel.Prelude.Maybe DocumentMetadata,
    s3Path :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    verificationMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    verificationStatus :: ResponseStatus,
    verificationUrl :: Kernel.Prelude.Maybe Kernel.Prelude.BaseUrl
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentVerificationConfigAPIEntity = DocumentVerificationConfigAPIEntity
  { applicableTo :: DocumentApplicableType,
    checkExpiry :: Kernel.Prelude.Bool,
    checkExtraction :: Kernel.Prelude.Bool,
    dependencyDocumentType :: [API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.DocumentType],
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disableWarning :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentCategory :: Kernel.Prelude.Maybe DocumentCategory,
    documentFields :: Kernel.Prelude.Maybe [FieldInfo],
    documentFlowGrouping :: DocumentFlowGrouping,
    documentOnboardingStage :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Endpoints.OnboardingExtra.DocumentOnboardingStage,
    documentType :: API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.DocumentType,
    filterForOldApks :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isApprovalSupported :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDisabled :: Kernel.Prelude.Bool,
    isHidden :: Kernel.Prelude.Bool,
    isMandatory :: Kernel.Prelude.Bool,
    isMandatoryForEnabling :: Kernel.Prelude.Bool,
    isReminderSupported :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    rcNumberPrefixList :: [Kernel.Prelude.Text],
    rolesAllowedToUploadDocument :: Kernel.Prelude.Maybe [API.Types.ProviderPlatform.Management.Endpoints.Account.DashboardAccessType],
    title :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentVerificationConfigList = DocumentVerificationConfigList
  { ambulances :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    autos :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    bikes :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    boat :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    bus :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    cabs :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    fleet :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    toto :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    trucks :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FieldInfo = FieldInfo
  { _type :: FieldType,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dropdownValues :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    isMandatory :: Kernel.Prelude.Bool,
    name :: Kernel.Prelude.Text,
    placeholder :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    regexValidation :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FieldType
  = FieldText
  | FieldInt
  | FieldDouble
  | FieldDropdown
  | FieldImage
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GSTDocumentMetadata = GSTDocumentMetadata {gstNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LDCDocumentMetadata = LDCDocumentMetadata {documentId :: Kernel.Prelude.Text, tdsRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocalAddressProofDocumentMetadata = LocalAddressProofDocumentMetadata
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    proofDocumentType :: Kernel.Prelude.Maybe Dashboard.Common.Driver.AddressDocumentType,
    state :: Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.IndianState
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NomineeDetailsDocumentMetadata = NomineeDetailsDocumentMetadata
  { nomineeDob :: Kernel.Prelude.Maybe Data.Time.Day,
    nomineeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    nomineeRelationship :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PanDocumentMetadata = PanDocumentMetadata
  { driverDob :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    panDocType :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.PanType,
    panNumber :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RCDocumentMetadata = RCDocumentMetadata
  { fitnessExpiry :: Kernel.Prelude.UTCTime,
    vehicleColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleManufacturer :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleModelYear :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleNumberPlate :: Kernel.Prelude.Text,
    vehicleVariant :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RcVerifyStatusResp = RcVerifyStatusResp {approved :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, documents :: [DocumentStatusItem], registrationNo :: Kernel.Prelude.Text, verified :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReferralInfoRes = ReferralInfoRes {name :: Kernel.Prelude.Text, personId :: Kernel.Types.Id.Id Dashboard.Common.Driver}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ResponseStatus
  = NO_DOC_AVAILABLE
  | PENDING
  | VALID
  | FAILED
  | INVALID
  | LIMIT_EXCEED
  | MANUAL_VERIFICATION_REQUIRED
  | UNAUTHORIZED
  | PULL_REQUIRED
  | CONSENT_DENIED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Role
  = NORMAL_FLEET
  | BUSINESS_FLEET
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data StatusRes = StatusRes
  { driverDocuments :: [DocumentStatusItem],
    driverLicenseDetails :: Kernel.Prelude.Maybe [API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.DLDetails],
    enabled :: Kernel.Prelude.Bool,
    manualVerificationRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    vehicleDocuments :: [VehicleDocumentItem],
    vehicleRegistrationCertificateDetails :: Kernel.Prelude.Maybe [API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.RCDetails]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TANDocumentMetadata = TANDocumentMetadata {documentId :: Kernel.Prelude.Text, tdsRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UDYAMDocumentMetadata = UDYAMDocumentMetadata {tdsRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double, udyamNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleDocumentItem = VehicleDocumentItem
  { dateOfUpload :: Kernel.Prelude.UTCTime,
    docsVerificationStatus :: Kernel.Prelude.Maybe Dashboard.Common.DocsVerificationStatus,
    documents :: [DocumentStatusItem],
    expiryDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    imageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isActive :: Kernel.Prelude.Bool,
    isApproved :: Kernel.Prelude.Bool,
    isVerified :: Kernel.Prelude.Bool,
    registrationNo :: Kernel.Prelude.Text,
    s3Path :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    userSelectedVehicleCategory :: Domain.Types.VehicleCategory.VehicleCategory,
    vehicleModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    verifiedVehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleDocumentStatusRes = VehicleDocumentStatusRes {vehicleDocument :: Kernel.Prelude.Maybe VehicleDocumentItem}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleFitnessCertificateDocumentMetadata = VehicleFitnessCertificateDocumentMetadata {applicationNumber :: Kernel.Prelude.Text, fitnessExpiry :: Kernel.Prelude.UTCTime, rcNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleInsuranceDocumentMetadata = VehicleInsuranceDocumentMetadata {insuranceExpiry :: Kernel.Prelude.UTCTime, insuranceProvider :: Kernel.Prelude.Text, policyNumber :: Kernel.Prelude.Text, rcNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehiclePUCDocumentMetadata = VehiclePUCDocumentMetadata {pucExpiry :: Kernel.Prelude.UTCTime, pucNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehiclePermitDocumentMetadata = VehiclePermitDocumentMetadata {permitExpiry :: Kernel.Prelude.UTCTime, permitNumber :: Kernel.Prelude.Text, rcNumber :: Kernel.Prelude.Text, regionCovered :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype VerifyDocumentRes = VerifyDocumentRes {enableFleetOwner :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VerifyReq = VerifyReq
  { driverId :: Kernel.Prelude.Text,
    identifierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    identifierNumber :: Kernel.Prelude.Text,
    imageId :: Kernel.Prelude.Text,
    optionalImageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VerifyType
  = VERIFY_PAN
  | VERIFY_GST
  | VERIFY_AADHAAR
  | VERIFY_UDYAM
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

type API = ("onboarding" :> (GetOnboardingDocumentConfigsHelper :<|> GetOnboardingRegisterStatusHelper :<|> GetOnboardingRegisterVehicleStatus :<|> PostOnboardingVerifyHelper :<|> GetOnboardingVehicleDocuments :<|> GetOnboardingGetReferralDetailsHelper))

type GetOnboardingDocumentConfigs =
  ( "document" :> "configs" :> QueryParam "makeSelfieAadhaarPanMandatory" Kernel.Prelude.Bool :> QueryParam "onlyVehicle" Kernel.Prelude.Bool
      :> QueryParam
           "role"
           Role
      :> Get '[JSON] DocumentVerificationConfigList
  )

type GetOnboardingDocumentConfigsHelper =
  ( "document" :> "configs" :> Capture "fleetOwnerId" Kernel.Prelude.Text
      :> QueryParam
           "makeSelfieAadhaarPanMandatory"
           Kernel.Prelude.Bool
      :> QueryParam "onlyVehicle" Kernel.Prelude.Bool
      :> QueryParam "role" Role
      :> Get '[JSON] DocumentVerificationConfigList
  )

type GetOnboardingRegisterStatus =
  ( "register" :> "status" :> QueryParam "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> QueryParam
           "makeSelfieAadhaarPanMandatory"
           Kernel.Prelude.Bool
      :> QueryParam "onboardingVehicleCategory" Domain.Types.VehicleCategory.VehicleCategory
      :> QueryParam
           "providePrefillDetails"
           Kernel.Prelude.Bool
      :> QueryParam
           "onlyMandatoryDocs"
           Kernel.Prelude.Bool
      :> QueryParam
           "docsVerificationStatus"
           Dashboard.Common.DocsVerificationStatus
      :> QueryParam
           "enableDocumentMetadata"
           Kernel.Prelude.Bool
      :> Get
           '[JSON]
           StatusRes
  )

type GetOnboardingRegisterStatusHelper =
  ( "register" :> "status" :> Capture "fleetOwnerId" Kernel.Prelude.Text
      :> QueryParam
           "driverId"
           (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> QueryParam "makeSelfieAadhaarPanMandatory" Kernel.Prelude.Bool
      :> QueryParam
           "onboardingVehicleCategory"
           Domain.Types.VehicleCategory.VehicleCategory
      :> QueryParam
           "providePrefillDetails"
           Kernel.Prelude.Bool
      :> QueryParam
           "onlyMandatoryDocs"
           Kernel.Prelude.Bool
      :> QueryParam
           "docsVerificationStatus"
           Dashboard.Common.DocsVerificationStatus
      :> QueryParam
           "enableDocumentMetadata"
           Kernel.Prelude.Bool
      :> Get
           '[JSON]
           StatusRes
  )

type GetOnboardingRegisterVehicleStatus =
  ( "register" :> "vehicleStatus" :> QueryParam "registrationNo" Kernel.Prelude.Text
      :> QueryParam
           "rcId"
           Kernel.Prelude.Text
      :> QueryParam "enableDocumentMetadata" Kernel.Prelude.Bool
      :> Get '[JSON] RcVerifyStatusResp
  )

type PostOnboardingVerify = ("verify" :> Capture "verifyType" VerifyType :> ReqBody '[JSON] VerifyReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostOnboardingVerifyHelper =
  ( "verify" :> Capture "verifyType" VerifyType
      :> QueryParam
           "accessType"
           API.Types.ProviderPlatform.Management.Endpoints.Account.DashboardAccessType
      :> QueryParam "adminApprovalRequired" Kernel.Prelude.Bool
      :> ReqBody '[JSON] VerifyReq
      :> Post
           '[JSON]
           VerifyDocumentRes
  )

type GetOnboardingVehicleDocuments =
  ( "vehicle" :> "documents" :> QueryParam "rcNo" Kernel.Prelude.Text :> QueryParam "rcId" Kernel.Prelude.Text
      :> QueryParam
           "enableDocumentMetadata"
           Kernel.Prelude.Bool
      :> Get '[JSON] VehicleDocumentStatusRes
  )

type GetOnboardingGetReferralDetails = ("get" :> "referralDetails" :> MandatoryQueryParam "referralCode" Kernel.Prelude.Text :> Get '[JSON] ReferralInfoRes)

type GetOnboardingGetReferralDetailsHelper =
  ( "get" :> "referralDetails" :> Capture "requestorId" Kernel.Prelude.Text :> MandatoryQueryParam "referralCode" Kernel.Prelude.Text
      :> Get
           '[JSON]
           ReferralInfoRes
  )

data OnboardingAPIs = OnboardingAPIs
  { getOnboardingDocumentConfigs :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Role -> EulerHS.Types.EulerClient DocumentVerificationConfigList,
    getOnboardingRegisterStatus :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Dashboard.Common.DocsVerificationStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> EulerHS.Types.EulerClient StatusRes,
    getOnboardingRegisterVehicleStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> EulerHS.Types.EulerClient RcVerifyStatusResp,
    postOnboardingVerify :: VerifyType -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Endpoints.Account.DashboardAccessType -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> VerifyReq -> EulerHS.Types.EulerClient VerifyDocumentRes,
    getOnboardingVehicleDocuments :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> EulerHS.Types.EulerClient VehicleDocumentStatusRes,
    getOnboardingGetReferralDetails :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient ReferralInfoRes
  }

mkOnboardingAPIs :: (Client EulerHS.Types.EulerClient API -> OnboardingAPIs)
mkOnboardingAPIs onboardingClient = (OnboardingAPIs {..})
  where
    getOnboardingDocumentConfigs :<|> getOnboardingRegisterStatus :<|> getOnboardingRegisterVehicleStatus :<|> postOnboardingVerify :<|> getOnboardingVehicleDocuments :<|> getOnboardingGetReferralDetails = onboardingClient

data OnboardingUserActionType
  = GET_ONBOARDING_DOCUMENT_CONFIGS
  | GET_ONBOARDING_REGISTER_STATUS
  | GET_ONBOARDING_REGISTER_VEHICLE_STATUS
  | POST_ONBOARDING_VERIFY
  | GET_ONBOARDING_VEHICLE_DOCUMENTS
  | GET_ONBOARDING_GET_REFERRAL_DETAILS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''Role)

$(mkHttpInstancesForEnum ''VerifyType)

$(Data.Singletons.TH.genSingletons [''OnboardingUserActionType])
