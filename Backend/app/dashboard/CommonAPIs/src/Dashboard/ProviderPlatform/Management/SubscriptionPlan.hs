{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Dashboard.ProviderPlatform.Management.SubscriptionPlan where

import Data.Aeson
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

-- | Plan entity with versioning and subscriber count info
data SubscriptionPlanEntity = SubscriptionPlanEntity
  { id :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Text,
    paymentMode :: Kernel.Prelude.Text,
    planType :: Kernel.Prelude.Text,
    billingType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    frequency :: Kernel.Prelude.Text,
    planBaseAmount :: Kernel.Prelude.Text,
    maxAmount :: Kernel.Types.Common.HighPrecMoney,
    registrationAmount :: Kernel.Types.Common.HighPrecMoney,
    cgstPercentage :: Kernel.Types.Common.HighPrecMoney,
    sgstPercentage :: Kernel.Types.Common.HighPrecMoney,
    freeRideCount :: Kernel.Prelude.Int,
    isOfferApplicable :: Kernel.Prelude.Bool,
    isDeprecated :: Kernel.Prelude.Bool,
    basedOnEntity :: Kernel.Prelude.Text,
    serviceName :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOpCityId :: Kernel.Prelude.Text,
    vehicleCategory :: Kernel.Prelude.Text,
    vehicleVariant :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    version :: Kernel.Prelude.Int,
    scheduledActivationDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    scheduledDeactivationDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    parentPlanId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    benefits :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    subscriberCount :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubscriptionPlanListResp = SubscriptionPlanListResp
  { plans :: [SubscriptionPlanEntity],
    totalCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype SubscriptionPlanDetailsResp = SubscriptionPlanDetailsResp
  { plan :: SubscriptionPlanEntity
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateSubscriptionPlanReq = CreateSubscriptionPlanReq
  { name :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Text,
    paymentMode :: Kernel.Prelude.Text,
    planType :: Kernel.Prelude.Text,
    billingType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    frequency :: Kernel.Prelude.Text,
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
    basedOnEntity :: Kernel.Prelude.Text,
    serviceName :: Kernel.Prelude.Text,
    vehicleVariant :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleCategory :: Kernel.Prelude.Text,
    listingPriority :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    validityInDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    isFleetOwnerPlan :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    benefits :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateSubscriptionPlanReq where
  hideSecrets = Kernel.Prelude.identity

newtype CreateSubscriptionPlanResp = CreateSubscriptionPlanResp {planId :: Kernel.Prelude.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateSubscriptionPlanReq = UpdateSubscriptionPlanReq
  { name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentMode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    planType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    billingType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    frequency :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    planBaseAmount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    maxAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    registrationAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    maxCreditLimit :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    maxMandateAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    productOwnershipAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cgstPercentage :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    sgstPercentage :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    freeRideCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    isOfferApplicable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDeprecated :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    basedOnEntity :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    benefits :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateSubscriptionPlanReq where
  hideSecrets = Kernel.Prelude.identity

newtype DeactivatePlanReq = DeactivatePlanReq
  { scheduledDeactivationDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DeactivatePlanReq where
  hideSecrets = Kernel.Prelude.identity

newtype ActivatePlanReq = ActivatePlanReq
  { scheduledActivationDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ActivatePlanReq where
  hideSecrets = Kernel.Prelude.identity

data PlanAnalyticsResp = PlanAnalyticsResp
  { planId :: Kernel.Prelude.Text,
    planName :: Kernel.Prelude.Text,
    totalSubscribers :: Kernel.Prelude.Int,
    activeSubscribers :: Kernel.Prelude.Int,
    totalRevenue :: Kernel.Types.Common.HighPrecMoney,
    currentVersion :: Kernel.Prelude.Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Servant API type definition
type API =
  "subscriptionPlan"
    :> ( GetSubscriptionPlanList
           :<|> GetSubscriptionPlanDetails
           :<|> PostSubscriptionPlanCreate
           :<|> PostSubscriptionPlanUpdate
           :<|> PostSubscriptionPlanDeactivate
           :<|> PostSubscriptionPlanActivate
           :<|> GetSubscriptionPlanAnalytics
       )

type GetSubscriptionPlanList =
  "list"
    :> QueryParam "status" Kernel.Prelude.Text
    :> QueryParam "city" Kernel.Prelude.Text
    :> QueryParam "frequency" Kernel.Prelude.Text
    :> QueryParam "limit" Kernel.Prelude.Int
    :> QueryParam "offset" Kernel.Prelude.Int
    :> Get '[JSON] SubscriptionPlanListResp

type GetSubscriptionPlanDetails =
  Capture "planId" Kernel.Prelude.Text
    :> "details"
    :> Get '[JSON] SubscriptionPlanDetailsResp

type PostSubscriptionPlanCreate =
  "create"
    :> ReqBody '[JSON] CreateSubscriptionPlanReq
    :> Post '[JSON] CreateSubscriptionPlanResp

type PostSubscriptionPlanUpdate =
  Capture "planId" Kernel.Prelude.Text
    :> "update"
    :> ReqBody '[JSON] UpdateSubscriptionPlanReq
    :> Post '[JSON] CreateSubscriptionPlanResp

type PostSubscriptionPlanDeactivate =
  Capture "planId" Kernel.Prelude.Text
    :> "deactivate"
    :> ReqBody '[JSON] DeactivatePlanReq
    :> Post '[JSON] Kernel.Types.APISuccess.APISuccess

type PostSubscriptionPlanActivate =
  Capture "planId" Kernel.Prelude.Text
    :> "activate"
    :> ReqBody '[JSON] ActivatePlanReq
    :> Post '[JSON] Kernel.Types.APISuccess.APISuccess

type GetSubscriptionPlanAnalytics =
  Capture "planId" Kernel.Prelude.Text
    :> "analytics"
    :> Get '[JSON] PlanAnalyticsResp
