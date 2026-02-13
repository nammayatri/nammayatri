module Domain.Action.ProviderPlatform.AppManagement.Subscription
  ( getSubscriptionListPlan,
    putSubscriptionSelectPlan,
    putSubscriptionSuspendPlan,
    postSubscriptionSubscribePlan,
    getSubscriptionCurrentPlan,
    getSubscriptionListPlanV2,
    putSubscriptionSelectPlanV2,
    putSubscriptionSuspendPlanV2,
    postSubscriptionSubscribePlanV2,
    getSubscriptionCurrentPlanV2,
    getSubscriptionOrderStatus,
    getSubscriptionDriverPaymentHistoryAPIV2,
    getSubscriptionDriverPaymentHistoryEntityDetailsV2,
    postSubscriptionCollectManualPayments,
    postSubscriptionFeeWaiveOff,
    getSubscriptionPurchaseList,
  )
where

import qualified API.Client.ProviderPlatform.AppManagement
import qualified API.Types.Dashboard.AppManagement.Subscription
import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified Domain.Action.UI.Driver
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Payment
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Plan
import qualified Domain.Types.Invoice
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Plan
import qualified "dynamic-offer-driver-app" Domain.Types.SubscriptionPurchase
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getSubscriptionListPlan ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Environment.Flow Domain.Action.UI.Plan.PlanListAPIRes
getSubscriptionListPlan merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
    checkedMerchantId
    opCity
    (.subscriptionDSL.getSubscriptionListPlan)
    driverId

putSubscriptionSelectPlan ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Kernel.Types.Id.Id Domain.Types.Plan.Plan ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putSubscriptionSelectPlan merchantShortId opCity apiTokenInfo driverId planId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      (Kernel.Prelude.Just driverId)
      Kernel.Prelude.Nothing
      SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
      checkedMerchantId
      opCity
      (.subscriptionDSL.putSubscriptionSelectPlan)
      driverId
      planId

putSubscriptionSuspendPlan ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putSubscriptionSuspendPlan merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      (Kernel.Prelude.Just driverId)
      Kernel.Prelude.Nothing
      SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
      checkedMerchantId
      opCity
      (.subscriptionDSL.putSubscriptionSuspendPlan)
      driverId

postSubscriptionSubscribePlan ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Kernel.Types.Id.Id Domain.Types.Plan.Plan ->
  Environment.Flow Domain.Action.UI.Plan.PlanSubscribeRes
postSubscriptionSubscribePlan merchantShortId opCity apiTokenInfo driverId planId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      (Kernel.Prelude.Just driverId)
      Kernel.Prelude.Nothing
      SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
      checkedMerchantId
      opCity
      (.subscriptionDSL.postSubscriptionSubscribePlan)
      driverId
      planId

getSubscriptionCurrentPlan ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Environment.Flow Domain.Action.UI.Plan.CurrentPlanRes
getSubscriptionCurrentPlan merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
    checkedMerchantId
    opCity
    (.subscriptionDSL.getSubscriptionCurrentPlan)
    driverId

getSubscriptionListPlanV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Domain.Types.Plan.ServiceNames ->
  Environment.Flow Domain.Action.UI.Plan.PlanListAPIRes
getSubscriptionListPlanV2 merchantShortId opCity apiTokenInfo driverId serviceName = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
    checkedMerchantId
    opCity
    (.subscriptionDSL.getSubscriptionListPlanV2)
    driverId
    serviceName

putSubscriptionSelectPlanV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Kernel.Types.Id.Id Domain.Types.Plan.Plan ->
  Domain.Types.Plan.ServiceNames ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putSubscriptionSelectPlanV2 merchantShortId opCity apiTokenInfo driverId planId serviceName = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      (Kernel.Prelude.Just driverId)
      Kernel.Prelude.Nothing
      SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
      checkedMerchantId
      opCity
      (.subscriptionDSL.putSubscriptionSelectPlanV2)
      driverId
      planId
      serviceName

putSubscriptionSuspendPlanV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Domain.Types.Plan.ServiceNames ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putSubscriptionSuspendPlanV2 merchantShortId opCity apiTokenInfo driverId serviceName = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      (Kernel.Prelude.Just driverId)
      Kernel.Prelude.Nothing
      SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
      checkedMerchantId
      opCity
      (.subscriptionDSL.putSubscriptionSuspendPlanV2)
      driverId
      serviceName

postSubscriptionSubscribePlanV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Kernel.Types.Id.Id Domain.Types.Plan.Plan ->
  Domain.Types.Plan.ServiceNames ->
  API.Types.Dashboard.AppManagement.Subscription.PlanSubscribeReq ->
  Environment.Flow Domain.Action.UI.Plan.PlanSubscribeRes
postSubscriptionSubscribePlanV2 merchantShortId opCity apiTokenInfo driverId planId serviceName req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      (Kernel.Prelude.Just driverId)
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
      checkedMerchantId
      opCity
      (.subscriptionDSL.postSubscriptionSubscribePlanV2)
      driverId
      planId
      serviceName
      req

getSubscriptionCurrentPlanV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Domain.Types.Plan.ServiceNames ->
  Environment.Flow Domain.Action.UI.Plan.CurrentPlanRes
getSubscriptionCurrentPlanV2 merchantShortId opCity apiTokenInfo driverId serviceName = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
    checkedMerchantId
    opCity
    (.subscriptionDSL.getSubscriptionCurrentPlanV2)
    driverId
    serviceName

getSubscriptionOrderStatus ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Kernel.Types.Id.Id Domain.Types.Invoice.Invoice ->
  Environment.Flow Domain.Action.UI.Payment.PaymentStatusResp
getSubscriptionOrderStatus merchantShortId opCity apiTokenInfo driverId orderId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
    checkedMerchantId
    opCity
    (.subscriptionDSL.getSubscriptionOrderStatus)
    driverId
    orderId

getSubscriptionDriverPaymentHistoryAPIV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Domain.Types.Plan.ServiceNames ->
  Kernel.Prelude.Maybe Domain.Types.Invoice.InvoicePaymentMode ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Environment.Flow Domain.Action.UI.Driver.HistoryEntityV2
getSubscriptionDriverPaymentHistoryAPIV2 merchantShortId opCity apiTokenInfo driverId serviceName paymentMode limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
    checkedMerchantId
    opCity
    (.subscriptionDSL.getSubscriptionDriverPaymentHistoryAPIV2)
    driverId
    serviceName
    paymentMode
    limit
    offset

getSubscriptionDriverPaymentHistoryEntityDetailsV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Domain.Types.Plan.ServiceNames ->
  Kernel.Types.Id.Id Domain.Types.Invoice.Invoice ->
  Environment.Flow Domain.Action.UI.Driver.HistoryEntryDetailsEntityV2
getSubscriptionDriverPaymentHistoryEntityDetailsV2 merchantShortId opCity apiTokenInfo driverId serviceName invoiceId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
    checkedMerchantId
    opCity
    (.subscriptionDSL.getSubscriptionDriverPaymentHistoryEntityDetailsV2)
    driverId
    serviceName
    invoiceId

postSubscriptionCollectManualPayments ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Domain.Types.Plan.ServiceNames ->
  API.Types.Dashboard.AppManagement.Subscription.CollectManualPaymentsReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postSubscriptionCollectManualPayments merchantShortId opCity apiTokenInfo driverId serviceName req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      (Kernel.Prelude.Just driverId)
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
      checkedMerchantId
      opCity
      (.subscriptionDSL.postSubscriptionCollectManualPayments)
      driverId
      serviceName
      req

postSubscriptionFeeWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.Subscription.WaiveOffReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postSubscriptionFeeWaiveOff merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.subscriptionDSL.postSubscriptionFeeWaiveOff) req

getSubscriptionPurchaseList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Domain.Types.SubscriptionPurchase.SubscriptionPurchaseStatus ->
  Environment.Flow Domain.Action.UI.Plan.SubscriptionPurchaseListRes
getSubscriptionPurchaseList merchantShortId opCity apiTokenInfo driverId limit offset status = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
    checkedMerchantId
    opCity
    (.subscriptionDSL.getSubscriptionPurchaseList)
    driverId
    limit
    offset
    status
