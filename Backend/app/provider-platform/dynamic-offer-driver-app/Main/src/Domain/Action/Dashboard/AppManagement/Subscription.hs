module Domain.Action.Dashboard.AppManagement.Subscription
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

import qualified API.Types.Dashboard.AppManagement.Subscription
import qualified "this" API.Types.Dashboard.RideBooking.Driver as Common
import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified Domain.Action.UI.Driver
import qualified Domain.Action.UI.Driver as Driver
import qualified Domain.Action.UI.Payment
import qualified "this" Domain.Action.UI.Plan
import qualified Domain.Types.DriverPlan as DDPlan
import qualified Domain.Types.Invoice
import qualified Domain.Types.Merchant
import Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import qualified Domain.Types.SubscriptionPurchase as DSP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as QPerson
import Tools.Error

getSubscriptionListPlan ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Environment.Flow Domain.Action.UI.Plan.PlanListAPIRes
getSubscriptionListPlan merchantShortId opCity driverId = do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  Domain.Action.UI.Plan.planList (Kernel.Types.Id.cast driverId, m.id, mOCityId) Domain.Types.Plan.YATRI_SUBSCRIPTION (Just 0) (Just 50) Nothing

putSubscriptionSelectPlan ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Kernel.Types.Id.Id Domain.Types.Plan.Plan ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putSubscriptionSelectPlan merchantShortId opCity driverId planId = do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  Domain.Action.UI.Plan.planSwitch Domain.Types.Plan.YATRI_SUBSCRIPTION planId (Kernel.Types.Id.cast driverId, m.id, mOCityId)

putSubscriptionSuspendPlan ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putSubscriptionSuspendPlan merchantShortId opCity driverId = do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  Domain.Action.UI.Plan.planSuspend Domain.Types.Plan.YATRI_SUBSCRIPTION True (Kernel.Types.Id.cast driverId, m.id, mOCityId)

postSubscriptionSubscribePlan ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Kernel.Types.Id.Id Domain.Types.Plan.Plan ->
  Environment.Flow Domain.Action.UI.Plan.PlanSubscribeRes
postSubscriptionSubscribePlan merchantShortId opCity driverId planId = do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  Domain.Action.UI.Plan.planSubscribe
    Domain.Types.Plan.YATRI_SUBSCRIPTION
    planId
    (True, Just DMM.WHATSAPP)
    (Kernel.Types.Id.cast driverId, m.id, mOCityId)
    DDPlan.NoData

getSubscriptionCurrentPlan ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Environment.Flow Domain.Action.UI.Plan.CurrentPlanRes
getSubscriptionCurrentPlan merchantShortId opCity driverId = do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  Domain.Action.UI.Plan.currentPlan Domain.Types.Plan.YATRI_SUBSCRIPTION (Kernel.Types.Id.cast driverId, m.id, mOCityId)

getSubscriptionListPlanV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Domain.Types.Plan.ServiceNames ->
  Environment.Flow Domain.Action.UI.Plan.PlanListAPIRes
getSubscriptionListPlanV2 merchantShortId opCity driverId serviceName = do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  Domain.Action.UI.Plan.planList (Kernel.Types.Id.cast driverId, m.id, mOCityId) serviceName (Just 0) (Just 50) Nothing

putSubscriptionSelectPlanV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Kernel.Types.Id.Id Domain.Types.Plan.Plan ->
  Domain.Types.Plan.ServiceNames ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putSubscriptionSelectPlanV2 merchantShortId opCity driverId planId serviceName = do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  Domain.Action.UI.Plan.planSwitch serviceName planId (Kernel.Types.Id.cast driverId, m.id, mOCityId)

putSubscriptionSuspendPlanV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Domain.Types.Plan.ServiceNames ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putSubscriptionSuspendPlanV2 merchantShortId opCity driverId serviceName = do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  Domain.Action.UI.Plan.planSuspend serviceName True (Kernel.Types.Id.cast driverId, m.id, mOCityId)

postSubscriptionSubscribePlanV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Kernel.Types.Id.Id Domain.Types.Plan.Plan ->
  Domain.Types.Plan.ServiceNames ->
  API.Types.Dashboard.AppManagement.Subscription.PlanSubscribeReq ->
  Environment.Flow Domain.Action.UI.Plan.PlanSubscribeRes
postSubscriptionSubscribePlanV2 merchantShortId opCity driverId planId serviceName req = do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  subscriptionRelatedData <- case req.vehicleNumber of
    Nothing -> return DDPlan.NoData
    Just vehicleNumber -> return $ DDPlan.RentedVehicleNumber vehicleNumber
  Domain.Action.UI.Plan.planSubscribe
    serviceName
    planId
    (True, Just DMM.WHATSAPP)
    (Kernel.Types.Id.cast driverId, m.id, mOCityId)
    subscriptionRelatedData

getSubscriptionCurrentPlanV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Domain.Types.Plan.ServiceNames ->
  Environment.Flow Domain.Action.UI.Plan.CurrentPlanRes
getSubscriptionCurrentPlanV2 merchantShortId opCity driverId serviceName = do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  Domain.Action.UI.Plan.currentPlan serviceName (Kernel.Types.Id.cast driverId, m.id, mOCityId)

getSubscriptionOrderStatus ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Kernel.Types.Id.Id Domain.Types.Invoice.Invoice ->
  Environment.Flow Domain.Action.UI.Payment.PaymentStatusResp
getSubscriptionOrderStatus merchantShortId opCity driverId invoiceId = do
  m <- findMerchantByShortId merchantShortId
  moCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  Domain.Action.UI.Payment.getStatus (Kernel.Types.Id.cast driverId, m.id, moCityId) (Kernel.Types.Id.cast invoiceId)

getSubscriptionDriverPaymentHistoryAPIV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Domain.Types.Plan.ServiceNames ->
  Kernel.Prelude.Maybe Domain.Types.Invoice.InvoicePaymentMode ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Environment.Flow Domain.Action.UI.Driver.HistoryEntityV2
getSubscriptionDriverPaymentHistoryAPIV2 merchantShortId opCity driverId serviceName invoicePaymentMode limit offset = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  Driver.getDriverPaymentsHistoryV2 (personId, merchant.id, merchantOpCityId) invoicePaymentMode limit offset serviceName

getSubscriptionDriverPaymentHistoryEntityDetailsV2 ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Domain.Types.Plan.ServiceNames ->
  Kernel.Types.Id.Id Domain.Types.Invoice.Invoice ->
  Environment.Flow Domain.Action.UI.Driver.HistoryEntryDetailsEntityV2
getSubscriptionDriverPaymentHistoryEntityDetailsV2 merchantShortId opCity driverId serviceName invoiceId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  Driver.getHistoryEntryDetailsEntityV2 (personId, merchant.id, merchantOpCityId) invoiceId.getId serviceName

postSubscriptionCollectManualPayments ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Domain.Types.Plan.ServiceNames ->
  API.Types.Dashboard.AppManagement.Subscription.CollectManualPaymentsReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postSubscriptionCollectManualPayments merchantShortId opCity driverId serviceName req = do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  let dataClearManualSelectedDues = Domain.Action.UI.Driver.ClearManualSelectedDues {driverFeeIds = fromMaybe [] req.paymentIds}
  _ <- Domain.Action.UI.Driver.clearDriverDues (Kernel.Types.Id.cast driverId, m.id, mOCityId) serviceName (Just dataClearManualSelectedDues) Nothing
  return Kernel.Types.APISuccess.Success

postSubscriptionFeeWaiveOff ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.Dashboard.AppManagement.Subscription.WaiveOffReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postSubscriptionFeeWaiveOff merchantShortId opCity req = do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  void $ Domain.Action.UI.Plan.updateWaiveOffByDriver mOCityId req.waiveOffEntities
  return Kernel.Types.APISuccess.Success

getSubscriptionPurchaseList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe DSP.SubscriptionPurchaseStatus ->
  Environment.Flow Domain.Action.UI.Plan.SubscriptionPurchaseListRes
getSubscriptionPurchaseList merchantShortId opCity driverId limit offset status = do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  Domain.Action.UI.Plan.subscriptionPurchaseList (Kernel.Types.Id.cast driverId, m.id, mOCityId) limit offset status
