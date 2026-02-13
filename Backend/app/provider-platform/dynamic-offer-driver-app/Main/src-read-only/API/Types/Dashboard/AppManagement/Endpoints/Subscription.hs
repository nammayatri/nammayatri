{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.Subscription where

import qualified API.Types.ProviderPlatform.Fleet.Driver
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Action.UI.Driver
import qualified "this" Domain.Action.UI.Payment
import qualified "this" Domain.Action.UI.Plan
import qualified Domain.Types.DriverFee
import qualified Domain.Types.DriverPlan
import qualified Domain.Types.Invoice
import qualified Domain.Types.Plan
import qualified "this" Domain.Types.SubscriptionPurchase
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data CollectManualPaymentsReq = CollectManualPaymentsReq {paymentIds :: Kernel.Prelude.Maybe [Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CollectManualPaymentsReq where
  hideSecrets = Kernel.Prelude.identity

newtype PlanSubscribeReq = PlanSubscribeReq {vehicleNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PlanSubscribeReq where
  hideSecrets = Kernel.Prelude.identity

newtype WaiveOffReq = WaiveOffReq {waiveOffEntities :: [Domain.Types.DriverPlan.WaiveOffEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets WaiveOffReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("plan" :> (GetSubscriptionListPlan :<|> PutSubscriptionSelectPlan :<|> PutSubscriptionSuspendPlan :<|> PostSubscriptionSubscribePlan :<|> GetSubscriptionCurrentPlan :<|> GetSubscriptionListPlanV2 :<|> PutSubscriptionSelectPlanV2 :<|> PutSubscriptionSuspendPlanV2 :<|> PostSubscriptionSubscribePlanV2 :<|> GetSubscriptionCurrentPlanV2 :<|> GetSubscriptionOrderStatus :<|> GetSubscriptionDriverPaymentHistoryAPIV2 :<|> GetSubscriptionDriverPaymentHistoryEntityDetailsV2 :<|> PostSubscriptionCollectManualPayments :<|> PostSubscriptionFeeWaiveOff :<|> GetSubscriptionPurchaseList))

type GetSubscriptionListPlan = (Capture "driverId" (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver) :> "list" :> Get '[JSON] Domain.Action.UI.Plan.PlanListAPIRes)

type PutSubscriptionSelectPlan =
  ( Capture "driverId" (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver)
      :> Capture
           "planId"
           (Kernel.Types.Id.Id Domain.Types.Plan.Plan)
      :> "select"
      :> Put '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PutSubscriptionSuspendPlan = (Capture "driverId" (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver) :> "suspend" :> Put '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostSubscriptionSubscribePlan =
  ( Capture "driverId" (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver)
      :> Capture
           "planId"
           (Kernel.Types.Id.Id Domain.Types.Plan.Plan)
      :> "subscribe"
      :> Post '[JSON] Domain.Action.UI.Plan.PlanSubscribeRes
  )

type GetSubscriptionCurrentPlan = (Capture "driverId" (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver) :> Get '[JSON] Domain.Action.UI.Plan.CurrentPlanRes)

type GetSubscriptionListPlanV2 =
  ( Capture "driverId" (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver)
      :> Capture
           "serviceName"
           Domain.Types.Plan.ServiceNames
      :> "v2"
      :> "list"
      :> Get '[JSON] Domain.Action.UI.Plan.PlanListAPIRes
  )

type PutSubscriptionSelectPlanV2 =
  ( Capture "driverId" (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver)
      :> Capture
           "planId"
           (Kernel.Types.Id.Id Domain.Types.Plan.Plan)
      :> Capture "serviceName" Domain.Types.Plan.ServiceNames
      :> "v2"
      :> "select"
      :> Put
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PutSubscriptionSuspendPlanV2 =
  ( Capture "driverId" (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver)
      :> Capture
           "serviceName"
           Domain.Types.Plan.ServiceNames
      :> "v2"
      :> "suspend"
      :> Put '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostSubscriptionSubscribePlanV2 =
  ( Capture "driverId" (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver)
      :> Capture
           "planId"
           (Kernel.Types.Id.Id Domain.Types.Plan.Plan)
      :> Capture "serviceName" Domain.Types.Plan.ServiceNames
      :> "v2"
      :> "subscribe"
      :> ReqBody
           '[JSON]
           PlanSubscribeReq
      :> Post
           '[JSON]
           Domain.Action.UI.Plan.PlanSubscribeRes
  )

type GetSubscriptionCurrentPlanV2 =
  ( Capture "driverId" (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver) :> Capture "serviceName" Domain.Types.Plan.ServiceNames
      :> Get
           '[JSON]
           Domain.Action.UI.Plan.CurrentPlanRes
  )

type GetSubscriptionOrderStatus =
  ( Capture "driverId" (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver)
      :> Capture
           "orderId"
           (Kernel.Types.Id.Id Domain.Types.Invoice.Invoice)
      :> "status"
      :> Get '[JSON] Domain.Action.UI.Payment.PaymentStatusResp
  )

type GetSubscriptionDriverPaymentHistoryAPIV2 =
  ( Capture "driverId" (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver) :> "payments" :> "history" :> "v2"
      :> Capture
           "serviceName"
           Domain.Types.Plan.ServiceNames
      :> QueryParam
           "paymentMode"
           Domain.Types.Invoice.InvoicePaymentMode
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           Domain.Action.UI.Driver.HistoryEntityV2
  )

type GetSubscriptionDriverPaymentHistoryEntityDetailsV2 =
  ( Capture
      "driverId"
      (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver)
      :> "payments"
      :> "history"
      :> "v2"
      :> Capture "serviceName" Domain.Types.Plan.ServiceNames
      :> Capture
           "invoiceId"
           (Kernel.Types.Id.Id Domain.Types.Invoice.Invoice)
      :> "entity"
      :> Get
           '[JSON]
           Domain.Action.UI.Driver.HistoryEntryDetailsEntityV2
  )

type PostSubscriptionCollectManualPayments =
  ( Capture "driverId" (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver)
      :> Capture
           "serviceName"
           Domain.Types.Plan.ServiceNames
      :> "collect"
      :> ReqBody '[JSON] CollectManualPaymentsReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostSubscriptionFeeWaiveOff = ("waiveOff" :> "fee" :> ReqBody '[JSON] WaiveOffReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetSubscriptionPurchaseList =
  ( Capture "driverId" (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver) :> "subscriptionPurchases"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "status"
           Domain.Types.SubscriptionPurchase.SubscriptionPurchaseStatus
      :> Get
           '[JSON]
           Domain.Action.UI.Plan.SubscriptionPurchaseListRes
  )

data SubscriptionAPIs = SubscriptionAPIs
  { getSubscriptionListPlan :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> EulerHS.Types.EulerClient Domain.Action.UI.Plan.PlanListAPIRes,
    putSubscriptionSelectPlan :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    putSubscriptionSuspendPlan :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postSubscriptionSubscribePlan :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> EulerHS.Types.EulerClient Domain.Action.UI.Plan.PlanSubscribeRes,
    getSubscriptionCurrentPlan :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> EulerHS.Types.EulerClient Domain.Action.UI.Plan.CurrentPlanRes,
    getSubscriptionListPlanV2 :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> EulerHS.Types.EulerClient Domain.Action.UI.Plan.PlanListAPIRes,
    putSubscriptionSelectPlanV2 :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Domain.Types.Plan.ServiceNames -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    putSubscriptionSuspendPlanV2 :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postSubscriptionSubscribePlanV2 :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Domain.Types.Plan.ServiceNames -> PlanSubscribeReq -> EulerHS.Types.EulerClient Domain.Action.UI.Plan.PlanSubscribeRes,
    getSubscriptionCurrentPlanV2 :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> EulerHS.Types.EulerClient Domain.Action.UI.Plan.CurrentPlanRes,
    getSubscriptionOrderStatus :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Invoice.Invoice -> EulerHS.Types.EulerClient Domain.Action.UI.Payment.PaymentStatusResp,
    getSubscriptionDriverPaymentHistoryAPIV2 :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Kernel.Prelude.Maybe Domain.Types.Invoice.InvoicePaymentMode -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient Domain.Action.UI.Driver.HistoryEntityV2,
    getSubscriptionDriverPaymentHistoryEntityDetailsV2 :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Kernel.Types.Id.Id Domain.Types.Invoice.Invoice -> EulerHS.Types.EulerClient Domain.Action.UI.Driver.HistoryEntryDetailsEntityV2,
    postSubscriptionCollectManualPayments :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> CollectManualPaymentsReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postSubscriptionFeeWaiveOff :: WaiveOffReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getSubscriptionPurchaseList :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.SubscriptionPurchase.SubscriptionPurchaseStatus -> EulerHS.Types.EulerClient Domain.Action.UI.Plan.SubscriptionPurchaseListRes
  }

mkSubscriptionAPIs :: (Client EulerHS.Types.EulerClient API -> SubscriptionAPIs)
mkSubscriptionAPIs subscriptionClient = (SubscriptionAPIs {..})
  where
    getSubscriptionListPlan :<|> putSubscriptionSelectPlan :<|> putSubscriptionSuspendPlan :<|> postSubscriptionSubscribePlan :<|> getSubscriptionCurrentPlan :<|> getSubscriptionListPlanV2 :<|> putSubscriptionSelectPlanV2 :<|> putSubscriptionSuspendPlanV2 :<|> postSubscriptionSubscribePlanV2 :<|> getSubscriptionCurrentPlanV2 :<|> getSubscriptionOrderStatus :<|> getSubscriptionDriverPaymentHistoryAPIV2 :<|> getSubscriptionDriverPaymentHistoryEntityDetailsV2 :<|> postSubscriptionCollectManualPayments :<|> postSubscriptionFeeWaiveOff :<|> getSubscriptionPurchaseList = subscriptionClient

data SubscriptionUserActionType
  = GET_SUBSCRIPTION_LIST_PLAN
  | PUT_SUBSCRIPTION_SELECT_PLAN
  | PUT_SUBSCRIPTION_SUSPEND_PLAN
  | POST_SUBSCRIPTION_SUBSCRIBE_PLAN
  | GET_SUBSCRIPTION_CURRENT_PLAN
  | GET_SUBSCRIPTION_LIST_PLAN_V2
  | PUT_SUBSCRIPTION_SELECT_PLAN_V2
  | PUT_SUBSCRIPTION_SUSPEND_PLAN_V2
  | POST_SUBSCRIPTION_SUBSCRIBE_PLAN_V2
  | GET_SUBSCRIPTION_CURRENT_PLAN_V2
  | GET_SUBSCRIPTION_ORDER_STATUS
  | GET_SUBSCRIPTION_DRIVER_PAYMENT_HISTORY_API_V2
  | GET_SUBSCRIPTION_DRIVER_PAYMENT_HISTORY_ENTITY_DETAILS_V2
  | POST_SUBSCRIPTION_COLLECT_MANUAL_PAYMENTS
  | POST_SUBSCRIPTION_FEE_WAIVE_OFF
  | GET_SUBSCRIPTION_PURCHASE_LIST
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''SubscriptionUserActionType])
