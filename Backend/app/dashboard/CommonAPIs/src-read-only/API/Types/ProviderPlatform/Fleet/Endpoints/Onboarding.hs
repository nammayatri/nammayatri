{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding where

import qualified API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration
import qualified Dashboard.Common
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.VehicleCategory
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Servant
import Servant.Client

data DocumentCategory
  = Driver
  | Vehicle
  | Permission
  | Training
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentStatusItem = DocumentStatusItem
  { documentType :: API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.DocumentType,
    verificationMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    verificationStatus :: ResponseStatus,
    verificationUrl :: Kernel.Prelude.Maybe Kernel.Prelude.BaseUrl
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentVerificationConfigAPIEntity = DocumentVerificationConfigAPIEntity
  { checkExpiry :: Kernel.Prelude.Bool,
    checkExtraction :: Kernel.Prelude.Bool,
    dependencyDocumentType :: [API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.DocumentType],
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disableWarning :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentCategory :: Kernel.Prelude.Maybe DocumentCategory,
    documentType :: API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.DocumentType,
    filterForOldApks :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDisabled :: Kernel.Prelude.Bool,
    isHidden :: Kernel.Prelude.Bool,
    isMandatory :: Kernel.Prelude.Bool,
    rcNumberPrefixList :: [Kernel.Prelude.Text],
    title :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentVerificationConfigList = DocumentVerificationConfigList
  { ambulances :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    autos :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    bikes :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    bus :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    cabs :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    fleet :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity],
    trucks :: Kernel.Prelude.Maybe [DocumentVerificationConfigAPIEntity]
  }
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

data VehicleDocumentItem = VehicleDocumentItem
  { dateOfUpload :: Kernel.Prelude.UTCTime,
    documents :: [DocumentStatusItem],
    isActive :: Kernel.Prelude.Bool,
    isVerified :: Kernel.Prelude.Bool,
    registrationNo :: Kernel.Prelude.Text,
    userSelectedVehicleCategory :: Domain.Types.VehicleCategory.VehicleCategory,
    vehicleModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    verifiedVehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("onboarding" :> (GetOnboardingDocumentConfigsHelper :<|> GetOnboardingRegisterStatusHelper))

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
      :> Get
           '[JSON]
           StatusRes
  )

data OnboardingAPIs = OnboardingAPIs
  { getOnboardingDocumentConfigs :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Role -> EulerHS.Types.EulerClient DocumentVerificationConfigList,
    getOnboardingRegisterStatus :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> EulerHS.Types.EulerClient StatusRes
  }

mkOnboardingAPIs :: (Client EulerHS.Types.EulerClient API -> OnboardingAPIs)
mkOnboardingAPIs onboardingClient = (OnboardingAPIs {..})
  where
    getOnboardingDocumentConfigs :<|> getOnboardingRegisterStatus = onboardingClient

data OnboardingUserActionType
  = GET_ONBOARDING_DOCUMENT_CONFIGS
  | GET_ONBOARDING_REGISTER_STATUS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''Role)

$(Data.Singletons.TH.genSingletons [''OnboardingUserActionType])
