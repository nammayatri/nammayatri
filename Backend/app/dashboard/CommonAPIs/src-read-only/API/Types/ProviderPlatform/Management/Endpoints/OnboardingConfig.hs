{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.OnboardingConfig where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding
import qualified API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.VehicleCategory
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Kernel.Utils.TH
import Servant
import Servant.Client

data ApplyConfigReq = ApplyConfigReq {config :: OnboardingConfig, changeNote :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ApplyConfigReq where
  hideSecrets = Kernel.Prelude.identity

data ApplyConfigRes = ApplyConfigRes {appliedAt :: Kernel.Prelude.UTCTime, changes :: [ConfigChange], issues :: [ValidationIssue]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChangeKind
  = ADDED
  | REMOVED
  | MODIFIED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data CloneConfigReq = CloneConfigReq
  { sourceMerchantShortId :: Kernel.Prelude.Text,
    sourceCity :: Kernel.Prelude.Text,
    sections :: [ConfigSection],
    documentTypes :: [API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.DocumentType]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CloneConfigReq where
  hideSecrets = Kernel.Prelude.identity

data CloneConfigRes = CloneConfigRes {sourceMerchantShortId :: Kernel.Prelude.Text, sourceCity :: Kernel.Prelude.Text, sections :: [ConfigSection], config :: OnboardingConfig}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ConfigChange = ConfigChange
  { kind :: ChangeKind,
    section :: ConfigSection,
    identifier :: Kernel.Prelude.Text,
    field :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    before :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    after :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ConfigSection
  = DRIVER_DOCUMENTS
  | FLEET_OWNER_DOCUMENTS
  | FLEET_BUSINESS_DOCUMENTS
  | STAGES
  deriving stock (Eq, Show, Generic, Bounded, Enum)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data DriverDocumentGroup = DriverDocumentGroup {vehicleCategory :: Domain.Types.VehicleCategory.VehicleCategory, documents :: [API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding.DocumentVerificationConfigAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EnumCatalog = EnumCatalog {name :: Kernel.Prelude.Text, values :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FieldPolicy = FieldPolicy {section :: ConfigSection, field :: Kernel.Prelude.Text, editable :: Kernel.Prelude.Bool, reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueSeverity
  = BLOCKING
  | WARNING
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data OnboardingConfig = OnboardingConfig
  { driverDocuments :: [DriverDocumentGroup],
    fleetOwnerDocuments :: [API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding.DocumentVerificationConfigAPIEntity],
    fleetBusinessDocuments :: [API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding.DocumentVerificationConfigAPIEntity],
    stages :: [StageGroup]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OnboardingConfigRes = OnboardingConfigRes {merchantOperatingCityId :: Kernel.Prelude.Text, cityName :: Kernel.Prelude.Text, config :: OnboardingConfig, enums :: [EnumCatalog], fieldPolicies :: [FieldPolicy]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StageGroup = StageGroup
  { documentCategory :: API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding.DocumentCategory,
    stages :: [API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding.DocumentOnboardingStageAPIEntity]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ValidationIssue = ValidationIssue
  { severity :: IssueSeverity,
    code :: Kernel.Prelude.Text,
    section :: ConfigSection,
    identifier :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    field :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    message :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("onboardingConfig" :> (GetOnboardingConfigGet :<|> PostOnboardingConfigClone :<|> PostOnboardingConfigApply))

type GetOnboardingConfigGet = ("get" :> Get ('[JSON]) OnboardingConfigRes)

type PostOnboardingConfigClone = ("clone" :> ReqBody ('[JSON]) CloneConfigReq :> Post ('[JSON]) CloneConfigRes)

type PostOnboardingConfigApply = ("apply" :> ReqBody ('[JSON]) ApplyConfigReq :> Post ('[JSON]) ApplyConfigRes)

data OnboardingConfigAPIs = OnboardingConfigAPIs
  { getOnboardingConfigGet :: (EulerHS.Types.EulerClient OnboardingConfigRes),
    postOnboardingConfigClone :: (CloneConfigReq -> EulerHS.Types.EulerClient CloneConfigRes),
    postOnboardingConfigApply :: (ApplyConfigReq -> EulerHS.Types.EulerClient ApplyConfigRes)
  }

mkOnboardingConfigAPIs :: (Client EulerHS.Types.EulerClient API -> OnboardingConfigAPIs)
mkOnboardingConfigAPIs onboardingConfigClient = (OnboardingConfigAPIs {..})
  where
    getOnboardingConfigGet :<|> postOnboardingConfigClone :<|> postOnboardingConfigApply = onboardingConfigClient

data OnboardingConfigUserActionType
  = GET_ONBOARDING_CONFIG_GET
  | POST_ONBOARDING_CONFIG_CLONE
  | POST_ONBOARDING_CONFIG_APPLY
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum (''ChangeKind))

$(mkHttpInstancesForEnum (''ConfigSection))

$(mkHttpInstancesForEnum (''IssueSeverity))

$(Data.Singletons.TH.genSingletons [(''OnboardingConfigUserActionType)])
