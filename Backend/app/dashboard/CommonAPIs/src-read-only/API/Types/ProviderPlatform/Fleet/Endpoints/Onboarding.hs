{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding where

import qualified API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration
import Data.Aeson
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
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

data Role
  = NORMAL_FLEET
  | BUSINESS_FLEET
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

type API = ("onboarding" :> GetOnboardingDocumentConfigsHelper)

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

newtype OnboardingAPIs = OnboardingAPIs {getOnboardingDocumentConfigs :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Role -> EulerHS.Types.EulerClient DocumentVerificationConfigList}

mkOnboardingAPIs :: (Client EulerHS.Types.EulerClient API -> OnboardingAPIs)
mkOnboardingAPIs onboardingClient = (OnboardingAPIs {..})
  where
    getOnboardingDocumentConfigs = onboardingClient

data OnboardingUserActionType
  = GET_ONBOARDING_DOCUMENT_CONFIGS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON OnboardingUserActionType where
  toJSON GET_ONBOARDING_DOCUMENT_CONFIGS = Data.Aeson.String "GET_ONBOARDING_DOCUMENT_CONFIGS"

instance FromJSON OnboardingUserActionType where
  parseJSON (Data.Aeson.String "GET_ONBOARDING_DOCUMENT_CONFIGS") = pure GET_ONBOARDING_DOCUMENT_CONFIGS
  parseJSON _ = fail "GET_ONBOARDING_DOCUMENT_CONFIGS expected"

$(mkHttpInstancesForEnum ''Role)

$(Data.Singletons.TH.genSingletons [''OnboardingUserActionType])
