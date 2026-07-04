{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.PlanManagement where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Kernel.Utils.TH
import Servant
import Servant.Client

data CreatePlanReq = CreatePlanReq
  { name :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Text,
    paymentMode :: PlanPaymentMode,
    planType :: PlanType,
    billingType :: Kernel.Prelude.Maybe PlanBillingType,
    frequency :: PlanFrequency,
    planBaseAmount :: Kernel.Prelude.Text,
    maxAmount :: Kernel.Types.Common.HighPrecMoney,
    registrationAmount :: Kernel.Types.Common.HighPrecMoney,
    originalRegistrationAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    airportRideSubscription :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    maxCreditLimit :: Kernel.Types.Common.HighPrecMoney,
    maxMandateAmount :: Kernel.Types.Common.HighPrecMoney,
    productOwnershipAmount :: Kernel.Types.Common.HighPrecMoney,
    cgstPercentage :: Kernel.Types.Common.HighPrecMoney,
    sgstPercentage :: Kernel.Types.Common.HighPrecMoney,
    freeRideCount :: Kernel.Prelude.Int,
    isOfferApplicable :: Kernel.Prelude.Bool,
    eligibleForCoinDiscount :: Kernel.Prelude.Bool,
    subscribedFlagToggleAllowed :: Kernel.Prelude.Bool,
    isDeprecated :: Kernel.Prelude.Bool,
    allowStrikeOff :: Kernel.Prelude.Bool,
    basedOnEntity :: PlanBasedOnEntity,
    serviceName :: Kernel.Prelude.Text,
    vehicleVariant :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleCategory :: Kernel.Prelude.Text,
    listingPriority :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    validityInDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    isFleetOwnerPlan :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreatePlanReq where
  hideSecrets = Kernel.Prelude.identity

newtype CreatePlanResp = CreatePlanResp {planId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ListPlansResp = ListPlansResp {plans :: [PlanAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PlanAPIEntity = PlanAPIEntity
  { id :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Text,
    paymentMode :: PlanPaymentMode,
    planType :: PlanType,
    billingType :: Kernel.Prelude.Maybe PlanBillingType,
    frequency :: PlanFrequency,
    planBaseAmount :: Kernel.Prelude.Text,
    maxAmount :: Kernel.Types.Common.HighPrecMoney,
    registrationAmount :: Kernel.Types.Common.HighPrecMoney,
    originalRegistrationAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    airportRideSubscription :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    maxCreditLimit :: Kernel.Types.Common.HighPrecMoney,
    maxMandateAmount :: Kernel.Types.Common.HighPrecMoney,
    productOwnershipAmount :: Kernel.Types.Common.HighPrecMoney,
    cgstPercentage :: Kernel.Types.Common.HighPrecMoney,
    sgstPercentage :: Kernel.Types.Common.HighPrecMoney,
    freeRideCount :: Kernel.Prelude.Int,
    isOfferApplicable :: Kernel.Prelude.Bool,
    eligibleForCoinDiscount :: Kernel.Prelude.Bool,
    subscribedFlagToggleAllowed :: Kernel.Prelude.Bool,
    isDeprecated :: Kernel.Prelude.Bool,
    allowStrikeOff :: Kernel.Prelude.Bool,
    basedOnEntity :: PlanBasedOnEntity,
    serviceName :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOpCityId :: Kernel.Prelude.Text,
    vehicleVariant :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleCategory :: Kernel.Prelude.Text,
    listingPriority :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    validityInDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    isFleetOwnerPlan :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PlanBasedOnEntity
  = RIDE
  | NONE
  | VEHICLE
  | VEHICLE_AND_RIDE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data PlanBillingType
  = PREPAID
  | POSTPAID
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data PlanFrequency
  = DAILY
  | WEEKLY
  | MONTHLY
  | FLEXIBLE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data PlanPaymentMode
  = MANUAL
  | AUTOPAY
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data PlanTranslationAPIEntity = PlanTranslationAPIEntity {planId :: Kernel.Prelude.Text, language :: Kernel.External.Types.Language, name :: Kernel.Prelude.Text, description :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PlanType
  = DEFAULT
  | SUBSCRIPTION
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

type API = ("planManagement" :> (PostPlanManagementCreate :<|> PostPlanManagementDeletePlan :<|> PostPlanManagementActivatePlan :<|> GetPlanManagementListPlans :<|> GetPlanManagementPlanTranslations))

type PostPlanManagementCreate = ("create" :> ReqBody '[JSON] CreatePlanReq :> Post '[JSON] CreatePlanResp)

type PostPlanManagementDeletePlan = (Capture "planId" Kernel.Prelude.Text :> "delete" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostPlanManagementActivatePlan = (Capture "planId" Kernel.Prelude.Text :> "activate" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetPlanManagementListPlans = ("list" :> QueryParam "serviceName" Kernel.Prelude.Text :> Get '[JSON] ListPlansResp)

type GetPlanManagementPlanTranslations = (Capture "planId" Kernel.Prelude.Text :> "translations" :> Get '[JSON] [PlanTranslationAPIEntity])

data PlanManagementAPIs = PlanManagementAPIs
  { postPlanManagementCreate :: CreatePlanReq -> EulerHS.Types.EulerClient CreatePlanResp,
    postPlanManagementDeletePlan :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPlanManagementActivatePlan :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getPlanManagementListPlans :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient ListPlansResp,
    getPlanManagementPlanTranslations :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient [PlanTranslationAPIEntity]
  }

mkPlanManagementAPIs :: (Client EulerHS.Types.EulerClient API -> PlanManagementAPIs)
mkPlanManagementAPIs planManagementClient = (PlanManagementAPIs {..})
  where
    postPlanManagementCreate :<|> postPlanManagementDeletePlan :<|> postPlanManagementActivatePlan :<|> getPlanManagementListPlans :<|> getPlanManagementPlanTranslations = planManagementClient

data PlanManagementUserActionType
  = POST_PLAN_MANAGEMENT_CREATE
  | POST_PLAN_MANAGEMENT_DELETE_PLAN
  | POST_PLAN_MANAGEMENT_ACTIVATE_PLAN
  | GET_PLAN_MANAGEMENT_LIST_PLANS
  | GET_PLAN_MANAGEMENT_PLAN_TRANSLATIONS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''PlanBasedOnEntity)

$(mkHttpInstancesForEnum ''PlanBillingType)

$(mkHttpInstancesForEnum ''PlanFrequency)

$(mkHttpInstancesForEnum ''PlanPaymentMode)

$(mkHttpInstancesForEnum ''PlanType)

$(Data.Singletons.TH.genSingletons [''PlanManagementUserActionType])
