{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.AppManagement.Subscription
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.Subscription
import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified Domain.Action.ProviderPlatform.AppManagement.Subscription
import qualified Domain.Action.UI.Driver
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Payment
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Plan
import qualified Domain.Types.Invoice
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Plan
import qualified "dynamic-offer-driver-app" Domain.Types.SubscriptionPurchase
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("plan" :> (GetSubscriptionListPlan :<|> PutSubscriptionSelectPlan :<|> PutSubscriptionSuspendPlan :<|> PostSubscriptionSubscribePlan :<|> GetSubscriptionCurrentPlan :<|> GetSubscriptionListPlanV2 :<|> PutSubscriptionSelectPlanV2 :<|> PutSubscriptionSuspendPlanV2 :<|> PostSubscriptionSubscribePlanV2 :<|> GetSubscriptionCurrentPlanV2 :<|> GetSubscriptionOrderStatus :<|> GetSubscriptionDriverPaymentHistoryAPIV2 :<|> GetSubscriptionDriverPaymentHistoryEntityDetailsV2 :<|> PostSubscriptionCollectManualPayments :<|> PostSubscriptionFeeWaiveOff :<|> GetSubscriptionPurchaseList))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getSubscriptionListPlan merchantId city :<|> putSubscriptionSelectPlan merchantId city :<|> putSubscriptionSuspendPlan merchantId city :<|> postSubscriptionSubscribePlan merchantId city :<|> getSubscriptionCurrentPlan merchantId city :<|> getSubscriptionListPlanV2 merchantId city :<|> putSubscriptionSelectPlanV2 merchantId city :<|> putSubscriptionSuspendPlanV2 merchantId city :<|> postSubscriptionSubscribePlanV2 merchantId city :<|> getSubscriptionCurrentPlanV2 merchantId city :<|> getSubscriptionOrderStatus merchantId city :<|> getSubscriptionDriverPaymentHistoryAPIV2 merchantId city :<|> getSubscriptionDriverPaymentHistoryEntityDetailsV2 merchantId city :<|> postSubscriptionCollectManualPayments merchantId city :<|> postSubscriptionFeeWaiveOff merchantId city :<|> getSubscriptionPurchaseList merchantId city

type GetSubscriptionListPlan =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.GET_SUBSCRIPTION_LIST_PLAN)
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionListPlan
  )

type PutSubscriptionSelectPlan =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.PUT_SUBSCRIPTION_SELECT_PLAN)
      :> API.Types.Dashboard.AppManagement.Subscription.PutSubscriptionSelectPlan
  )

type PutSubscriptionSuspendPlan =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.PUT_SUBSCRIPTION_SUSPEND_PLAN)
      :> API.Types.Dashboard.AppManagement.Subscription.PutSubscriptionSuspendPlan
  )

type PostSubscriptionSubscribePlan =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.POST_SUBSCRIPTION_SUBSCRIBE_PLAN)
      :> API.Types.Dashboard.AppManagement.Subscription.PostSubscriptionSubscribePlan
  )

type GetSubscriptionCurrentPlan =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.GET_SUBSCRIPTION_CURRENT_PLAN)
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionCurrentPlan
  )

type GetSubscriptionListPlanV2 =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.GET_SUBSCRIPTION_LIST_PLAN_V2)
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionListPlanV2
  )

type PutSubscriptionSelectPlanV2 =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.PUT_SUBSCRIPTION_SELECT_PLAN_V2)
      :> API.Types.Dashboard.AppManagement.Subscription.PutSubscriptionSelectPlanV2
  )

type PutSubscriptionSuspendPlanV2 =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.PUT_SUBSCRIPTION_SUSPEND_PLAN_V2)
      :> API.Types.Dashboard.AppManagement.Subscription.PutSubscriptionSuspendPlanV2
  )

type PostSubscriptionSubscribePlanV2 =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.POST_SUBSCRIPTION_SUBSCRIBE_PLAN_V2)
      :> API.Types.Dashboard.AppManagement.Subscription.PostSubscriptionSubscribePlanV2
  )

type GetSubscriptionCurrentPlanV2 =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.GET_SUBSCRIPTION_CURRENT_PLAN_V2)
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionCurrentPlanV2
  )

type GetSubscriptionOrderStatus =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.GET_SUBSCRIPTION_ORDER_STATUS)
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionOrderStatus
  )

type GetSubscriptionDriverPaymentHistoryAPIV2 =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.GET_SUBSCRIPTION_DRIVER_PAYMENT_HISTORY_API_V2)
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionDriverPaymentHistoryAPIV2
  )

type GetSubscriptionDriverPaymentHistoryEntityDetailsV2 =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.GET_SUBSCRIPTION_DRIVER_PAYMENT_HISTORY_ENTITY_DETAILS_V2)
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionDriverPaymentHistoryEntityDetailsV2
  )

type PostSubscriptionCollectManualPayments =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.POST_SUBSCRIPTION_COLLECT_MANUAL_PAYMENTS)
      :> API.Types.Dashboard.AppManagement.Subscription.PostSubscriptionCollectManualPayments
  )

type PostSubscriptionFeeWaiveOff =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.POST_SUBSCRIPTION_FEE_WAIVE_OFF)
      :> API.Types.Dashboard.AppManagement.Subscription.PostSubscriptionFeeWaiveOff
  )

type GetSubscriptionPurchaseList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.Subscription.GET_SUBSCRIPTION_PURCHASE_LIST)
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionPurchaseList
  )

getSubscriptionListPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Environment.FlowHandler Domain.Action.UI.Plan.PlanListAPIRes)
getSubscriptionListPlan merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.getSubscriptionListPlan merchantShortId opCity apiTokenInfo driverId

putSubscriptionSelectPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putSubscriptionSelectPlan merchantShortId opCity apiTokenInfo driverId planId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.putSubscriptionSelectPlan merchantShortId opCity apiTokenInfo driverId planId

putSubscriptionSuspendPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putSubscriptionSuspendPlan merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.putSubscriptionSuspendPlan merchantShortId opCity apiTokenInfo driverId

postSubscriptionSubscribePlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Environment.FlowHandler Domain.Action.UI.Plan.PlanSubscribeRes)
postSubscriptionSubscribePlan merchantShortId opCity apiTokenInfo driverId planId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.postSubscriptionSubscribePlan merchantShortId opCity apiTokenInfo driverId planId

getSubscriptionCurrentPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Environment.FlowHandler Domain.Action.UI.Plan.CurrentPlanRes)
getSubscriptionCurrentPlan merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.getSubscriptionCurrentPlan merchantShortId opCity apiTokenInfo driverId

getSubscriptionListPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Environment.FlowHandler Domain.Action.UI.Plan.PlanListAPIRes)
getSubscriptionListPlanV2 merchantShortId opCity apiTokenInfo driverId serviceName = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.getSubscriptionListPlanV2 merchantShortId opCity apiTokenInfo driverId serviceName

putSubscriptionSelectPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Domain.Types.Plan.ServiceNames -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putSubscriptionSelectPlanV2 merchantShortId opCity apiTokenInfo driverId planId serviceName = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.putSubscriptionSelectPlanV2 merchantShortId opCity apiTokenInfo driverId planId serviceName

putSubscriptionSuspendPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putSubscriptionSuspendPlanV2 merchantShortId opCity apiTokenInfo driverId serviceName = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.putSubscriptionSuspendPlanV2 merchantShortId opCity apiTokenInfo driverId serviceName

postSubscriptionSubscribePlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Domain.Types.Plan.ServiceNames -> API.Types.Dashboard.AppManagement.Subscription.PlanSubscribeReq -> Environment.FlowHandler Domain.Action.UI.Plan.PlanSubscribeRes)
postSubscriptionSubscribePlanV2 merchantShortId opCity apiTokenInfo driverId planId serviceName req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.postSubscriptionSubscribePlanV2 merchantShortId opCity apiTokenInfo driverId planId serviceName req

getSubscriptionCurrentPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Environment.FlowHandler Domain.Action.UI.Plan.CurrentPlanRes)
getSubscriptionCurrentPlanV2 merchantShortId opCity apiTokenInfo driverId serviceName = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.getSubscriptionCurrentPlanV2 merchantShortId opCity apiTokenInfo driverId serviceName

getSubscriptionOrderStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Invoice.Invoice -> Environment.FlowHandler Domain.Action.UI.Payment.PaymentStatusResp)
getSubscriptionOrderStatus merchantShortId opCity apiTokenInfo driverId orderId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.getSubscriptionOrderStatus merchantShortId opCity apiTokenInfo driverId orderId

getSubscriptionDriverPaymentHistoryAPIV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Kernel.Prelude.Maybe Domain.Types.Invoice.InvoicePaymentMode -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler Domain.Action.UI.Driver.HistoryEntityV2)
getSubscriptionDriverPaymentHistoryAPIV2 merchantShortId opCity apiTokenInfo driverId serviceName paymentMode limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.getSubscriptionDriverPaymentHistoryAPIV2 merchantShortId opCity apiTokenInfo driverId serviceName paymentMode limit offset

getSubscriptionDriverPaymentHistoryEntityDetailsV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Kernel.Types.Id.Id Domain.Types.Invoice.Invoice -> Environment.FlowHandler Domain.Action.UI.Driver.HistoryEntryDetailsEntityV2)
getSubscriptionDriverPaymentHistoryEntityDetailsV2 merchantShortId opCity apiTokenInfo driverId serviceName invoiceId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.getSubscriptionDriverPaymentHistoryEntityDetailsV2 merchantShortId opCity apiTokenInfo driverId serviceName invoiceId

postSubscriptionCollectManualPayments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> API.Types.Dashboard.AppManagement.Subscription.CollectManualPaymentsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSubscriptionCollectManualPayments merchantShortId opCity apiTokenInfo driverId serviceName req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.postSubscriptionCollectManualPayments merchantShortId opCity apiTokenInfo driverId serviceName req

postSubscriptionFeeWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.Subscription.WaiveOffReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSubscriptionFeeWaiveOff merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.postSubscriptionFeeWaiveOff merchantShortId opCity apiTokenInfo req

getSubscriptionPurchaseList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.SubscriptionPurchase.SubscriptionPurchaseStatus -> Environment.FlowHandler Domain.Action.UI.Plan.SubscriptionPurchaseListRes)
getSubscriptionPurchaseList merchantShortId opCity apiTokenInfo driverId limit offset status = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Subscription.getSubscriptionPurchaseList merchantShortId opCity apiTokenInfo driverId limit offset status
