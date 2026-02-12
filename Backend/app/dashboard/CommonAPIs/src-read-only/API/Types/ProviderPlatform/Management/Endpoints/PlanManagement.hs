{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.PlanManagement where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
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

data PlanType
  = DEFAULT
  | SUBSCRIPTION
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

type API = ("planManagement" :> (PostPlanManagementCreate :<|> GetPlanManagementPlan :<|> PostPlanManagementDeletePlan :<|> GetPlanManagementListPlans))

type PostPlanManagementCreate = ("create" :> ReqBody '[JSON] CreatePlanReq :> Post '[JSON] CreatePlanResp)

type GetPlanManagementPlan = (Capture "planId" Kernel.Prelude.Text :> Get '[JSON] PlanAPIEntity)

type PostPlanManagementDeletePlan = (Capture "planId" Kernel.Prelude.Text :> "delete" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetPlanManagementListPlans = ("list" :> QueryParam "serviceName" Kernel.Prelude.Text :> Get '[JSON] ListPlansResp)

data PlanManagementAPIs = PlanManagementAPIs
  { postPlanManagementCreate :: CreatePlanReq -> EulerHS.Types.EulerClient CreatePlanResp,
    getPlanManagementPlan :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient PlanAPIEntity,
    postPlanManagementDeletePlan :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getPlanManagementListPlans :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient ListPlansResp
  }

mkPlanManagementAPIs :: (Client EulerHS.Types.EulerClient API -> PlanManagementAPIs)
mkPlanManagementAPIs planManagementClient = (PlanManagementAPIs {..})
  where
    postPlanManagementCreate :<|> getPlanManagementPlan :<|> postPlanManagementDeletePlan :<|> getPlanManagementListPlans = planManagementClient

data PlanManagementUserActionType
  = POST_PLAN_MANAGEMENT_CREATE
  | GET_PLAN_MANAGEMENT_PLAN
  | POST_PLAN_MANAGEMENT_DELETE_PLAN
  | GET_PLAN_MANAGEMENT_LIST_PLANS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''PlanBasedOnEntity)

$(mkHttpInstancesForEnum ''PlanBillingType)

$(mkHttpInstancesForEnum ''PlanFrequency)

$(mkHttpInstancesForEnum ''PlanPaymentMode)

$(mkHttpInstancesForEnum ''PlanType)

$(Data.Singletons.TH.genSingletons [''PlanManagementUserActionType])
