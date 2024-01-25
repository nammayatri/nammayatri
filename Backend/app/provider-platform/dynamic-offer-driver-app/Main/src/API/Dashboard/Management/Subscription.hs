{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Management.Subscription where

import qualified "dashboard-helper-api" Dashboard.Common as DP
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Action.UI.Driver as Driver
import qualified Domain.Action.UI.Payment as APayment
import qualified Domain.Action.UI.Plan as DTPlan
import qualified Domain.Types.DriverPlan as DDPlan
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Merchant as DM
import Domain.Types.Merchant.MerchantMessage as DMM
import qualified Domain.Types.Plan as DPlan
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import SharedLogic.Merchant
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverInformation as DI

data SubscriptionEndpoint
  = SelectPlanEndpoint
  | SubscribePlanEndpoint
  | SuspendPlanEndpoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord)

derivePersistField "SubscriptionEndpoint"

type API =
  "plan"
    :> ( ListPlanV2
           :<|> SelectPlanV2
           :<|> SuspendPlanV2
           :<|> SubscribePlanV2
           :<|> CurrentPlanV2
           :<|> ListPlan
           :<|> SelectPlan
           :<|> SuspendPlan
           :<|> SubscribePlan
           :<|> CurrentPlan
           :<|> OrderStatus
           :<|> DriverPaymentHistoryAPIV2
           :<|> DriverPaymentHistoryEntityDetailsAPIV2
           :<|> DriverPaymentHistoryAPI
           :<|> DriverPaymentHistoryEntityDetailsAPI
           :<|> Common.UpdateSubscriptionDriverFeeAndInvoiceAPI
           :<|> SendSmsToDriverAPI
       )

type ListPlan =
  Capture "driverId" (Id DP.Driver)
    :> "list"
    :> Get '[JSON] DTPlan.PlanListAPIRes

type ListPlanV2 =
  Capture "driverId" (Id DP.Driver)
    :> Capture "serviceName" DPlan.ServiceNames
    :> "v2"
    :> "list"
    :> Get '[JSON] DTPlan.PlanListAPIRes

type SelectPlan =
  Capture "driverId" (Id DP.Driver)
    :> Capture "planId" (Id DPlan.Plan)
    :> "select"
    :> Put '[JSON] APISuccess

type SelectPlanV2 =
  Capture "driverId" (Id DP.Driver)
    :> Capture "planId" (Id DPlan.Plan)
    :> Capture "serviceName" DPlan.ServiceNames
    :> "v2"
    :> "select"
    :> Put '[JSON] APISuccess

type SuspendPlan =
  Capture "driverId" (Id DP.Driver)
    :> "suspend"
    :> Put '[JSON] APISuccess

type SuspendPlanV2 =
  Capture "driverId" (Id DP.Driver)
    :> Capture "serviceName" DPlan.ServiceNames
    :> "v2"
    :> "suspend"
    :> Put '[JSON] APISuccess

type SubscribePlanV2 =
  Capture "driverId" (Id DP.Driver)
    :> Capture "planId" (Id DPlan.Plan)
    :> Capture "serviceName" DPlan.ServiceNames
    :> "v2"
    :> "subscribe"
    :> ReqBody '[JSON] PlanSubscribeReq
    :> Post '[JSON] DTPlan.PlanSubscribeRes

type SubscribePlan =
  Capture "driverId" (Id DP.Driver)
    :> Capture "planId" (Id DPlan.Plan)
    :> "subscribe"
    :> Post '[JSON] DTPlan.PlanSubscribeRes

newtype PlanSubscribeReq = PlanSubscribeReq
  { vehicleNumber :: Maybe Text
  }
  deriving (Generic, Show, Read, FromJSON, ToJSON, Eq, Ord, ToSchema)

type CurrentPlanV2 =
  Capture "driverId" (Id DP.Driver)
    :> Capture "serviceName" DPlan.ServiceNames
    :> Get '[JSON] DTPlan.CurrentPlanRes

type CurrentPlan =
  Capture "driverId" (Id DP.Driver)
    :> Get '[JSON] DTPlan.CurrentPlanRes

type OrderStatus =
  Capture "driverId" (Id DP.Driver)
    :> Capture "orderId" (Id INV.Invoice)
    :> "status"
    :> Get '[JSON] APayment.PaymentStatusResp

----- payment history ----------
type DriverPaymentHistoryAPIV2 =
  Capture "driverId" (Id Common.Driver)
    :> "payments"
    :> "history"
    :> "v2"
    :> Capture "serviceName" DPlan.ServiceNames
    :> QueryParam "paymentMode" INV.InvoicePaymentMode
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] Driver.HistoryEntityV2

type DriverPaymentHistoryAPI =
  Capture "driverId" (Id Common.Driver)
    :> "payments"
    :> "history"
    :> QueryParam "paymentMode" INV.InvoicePaymentMode
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] Driver.HistoryEntityV2

----------- payment history entry  -------------
type DriverPaymentHistoryEntityDetailsAPIV2 =
  Capture "driverId" (Id Common.Driver)
    :> "payments"
    :> "history"
    :> "v2"
    :> Capture "serviceName" DPlan.ServiceNames
    :> Capture "invoiceId" (Id INV.Invoice)
    :> "entity"
    :> Get '[JSON] Driver.HistoryEntryDetailsEntityV2

type DriverPaymentHistoryEntityDetailsAPI =
  Capture "driverId" (Id Common.Driver)
    :> "payments"
    :> "history"
    :> Capture "invoiceId" (Id INV.Invoice)
    :> "entity"
    :> Get '[JSON] Driver.HistoryEntryDetailsEntityV2

-------------------------------------------------------------------
------- Send Sms to Driver ----------------------------------------

type SendMessageToDriverViaDashboardAPI =
  Capture "driverId" (Id Common.Driver)
    :> "sendSms"
    :> ReqBody '[JSON] DDriver.SendSmsReq
    :> Post '[JSON] APISuccess

type SendSmsToDriverAPI =
  Capture "driverId" (Id Common.Driver)
    :> Capture "volunteerId" Text
    :> "sendSms"
    :> ReqBody '[JSON] DDriver.SendSmsReq
    :> Post '[JSON] APISuccess

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city =
  planListV2 merchantId city
    :<|> planSelectV2 merchantId city
    :<|> planSuspendV2 merchantId city
    :<|> planSubscribeV2 merchantId city
    :<|> currentPlanV2 merchantId city
    :<|> planList merchantId city
    :<|> planSelect merchantId city
    :<|> planSuspend merchantId city
    :<|> planSubscribe merchantId city
    :<|> currentPlan merchantId city
    :<|> paymentStatus merchantId city
    :<|> getPaymentHistoryV2 merchantId city
    :<|> getPaymentHistoryEntityDetailsV2 merchantId city
    :<|> getPaymentHistory merchantId city
    :<|> getPaymentHistoryEntityDetails merchantId city
    :<|> updateDriverSubscriptionDriverFeeAndInvoiceUpdate merchantId city
    :<|> sendSmsToDriver merchantId city

planListV2 :: ShortId DM.Merchant -> Context.City -> Id DP.Driver -> DPlan.ServiceNames -> FlowHandler DTPlan.PlanListAPIRes
planListV2 merchantShortId opCity driverId serviceName = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  mOCityId <- withFlowHandlerAPI $ CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  withFlowHandlerAPI $ DTPlan.planList (cast driverId, m.id, mOCityId) serviceName (Just 0) (Just 50)

planList :: ShortId DM.Merchant -> Context.City -> Id DP.Driver -> FlowHandler DTPlan.PlanListAPIRes
planList merchantShortId opCity driverId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  mOCityId <- withFlowHandlerAPI $ CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  withFlowHandlerAPI $ DTPlan.planList (cast driverId, m.id, mOCityId) DPlan.YATRI_SUBSCRIPTION (Just 0) (Just 50)

planSelectV2 :: ShortId DM.Merchant -> Context.City -> Id DP.Driver -> Id DPlan.Plan -> DPlan.ServiceNames -> FlowHandler APISuccess
planSelectV2 merchantShortId opCity driverId planId serviceName = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  mOCityId <- withFlowHandlerAPI $ CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  withFlowHandlerAPI $ DTPlan.planSwitch serviceName planId (cast driverId, m.id, mOCityId)

planSelect :: ShortId DM.Merchant -> Context.City -> Id DP.Driver -> Id DPlan.Plan -> FlowHandler APISuccess
planSelect merchantShortId opCity driverId planId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  mOCityId <- withFlowHandlerAPI $ CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  withFlowHandlerAPI $ DTPlan.planSwitch DPlan.YATRI_SUBSCRIPTION planId (cast driverId, m.id, mOCityId)

planSuspendV2 :: ShortId DM.Merchant -> Context.City -> Id DP.Driver -> DPlan.ServiceNames -> FlowHandler APISuccess
planSuspendV2 merchantShortId opCity driverId serviceName = withFlowHandlerAPI $ do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  DTPlan.planSuspend serviceName True (cast driverId, m.id, mOCityId)

planSuspend :: ShortId DM.Merchant -> Context.City -> Id DP.Driver -> FlowHandler APISuccess
planSuspend merchantShortId opCity driverId = withFlowHandlerAPI $ do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  DTPlan.planSuspend DPlan.YATRI_SUBSCRIPTION True (cast driverId, m.id, mOCityId)

planSubscribeV2 ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DP.Driver ->
  Id DPlan.Plan ->
  DPlan.ServiceNames ->
  PlanSubscribeReq ->
  FlowHandler DTPlan.PlanSubscribeRes
planSubscribeV2 merchantShortId opCity driverId planId serviceName req = withFlowHandlerAPI $ do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  driverInfo <- DI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  subscriptionRelatedData <- case req.vehicleNumber of
    Nothing -> return DDPlan.NoData
    Just vehicleNumber -> return $ DDPlan.RentedVehicleNumber vehicleNumber
  DTPlan.planSubscribe serviceName planId (True, Just DMM.WHATSAPP) (cast driverId, m.id, mOCityId) driverInfo subscriptionRelatedData

planSubscribe ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DP.Driver ->
  Id DPlan.Plan ->
  FlowHandler DTPlan.PlanSubscribeRes
planSubscribe merchantShortId opCity driverId planId = withFlowHandlerAPI $ do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  driverInfo <- DI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  DTPlan.planSubscribe DPlan.YATRI_SUBSCRIPTION planId (True, Just DMM.WHATSAPP) (cast driverId, m.id, mOCityId) driverInfo DDPlan.NoData

currentPlanV2 :: ShortId DM.Merchant -> Context.City -> Id DP.Driver -> DPlan.ServiceNames -> FlowHandler DTPlan.CurrentPlanRes
currentPlanV2 merchantShortId opCity driverId serviceName = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  mOCityId <- withFlowHandlerAPI $ CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  withFlowHandlerAPI $ DTPlan.currentPlan serviceName (cast driverId, m.id, mOCityId)

currentPlan :: ShortId DM.Merchant -> Context.City -> Id DP.Driver -> FlowHandler DTPlan.CurrentPlanRes
currentPlan merchantShortId opCity driverId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  mOCityId <- withFlowHandlerAPI $ CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  withFlowHandlerAPI $ DTPlan.currentPlan DPlan.YATRI_SUBSCRIPTION (cast driverId, m.id, mOCityId)

paymentStatus :: ShortId DM.Merchant -> Context.City -> Id DP.Driver -> Id INV.Invoice -> FlowHandler APayment.PaymentStatusResp
paymentStatus merchantShortId opCity driverId invoiceId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  moCityId <- withFlowHandlerAPI $ CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  withFlowHandlerAPI $ APayment.getStatus (cast driverId, m.id, moCityId) (cast invoiceId)

getPaymentHistory ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Maybe INV.InvoicePaymentMode ->
  Maybe Int ->
  Maybe Int ->
  FlowHandler Driver.HistoryEntityV2
getPaymentHistory merchantShortId opCity driverId invoicePaymentMode limit offset = withFlowHandlerAPI $ DDriver.getPaymentHistory merchantShortId opCity driverId invoicePaymentMode limit offset DPlan.YATRI_SUBSCRIPTION

getPaymentHistoryV2 ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  DPlan.ServiceNames ->
  Maybe INV.InvoicePaymentMode ->
  Maybe Int ->
  Maybe Int ->
  FlowHandler Driver.HistoryEntityV2
getPaymentHistoryV2 merchantShortId opCity driverId serviceName invoicePaymentMode limit offset = withFlowHandlerAPI $ DDriver.getPaymentHistory merchantShortId opCity driverId invoicePaymentMode limit offset serviceName

getPaymentHistoryEntityDetailsV2 ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  DPlan.ServiceNames ->
  Id INV.Invoice ->
  FlowHandler Driver.HistoryEntryDetailsEntityV2
getPaymentHistoryEntityDetailsV2 merchantShortId opCity driverId serviceName invoiceId = do
  withFlowHandlerAPI $ DDriver.getPaymentHistoryEntityDetails merchantShortId opCity driverId invoiceId serviceName

getPaymentHistoryEntityDetails ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Id INV.Invoice ->
  FlowHandler Driver.HistoryEntryDetailsEntityV2
getPaymentHistoryEntityDetails merchantShortId opCity driverId invoiceId = do
  withFlowHandlerAPI $ DDriver.getPaymentHistoryEntityDetails merchantShortId opCity driverId invoiceId DPlan.YATRI_SUBSCRIPTION

updateDriverSubscriptionDriverFeeAndInvoiceUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Common.ServiceNames ->
  Common.SubscriptionDriverFeesAndInvoicesToUpdate ->
  FlowHandler Common.SubscriptionDriverFeesAndInvoicesToUpdate
updateDriverSubscriptionDriverFeeAndInvoiceUpdate merchantShortId opCity driverId serviceName req = withFlowHandlerAPI $ DDriver.updateSubscriptionDriverFeeAndInvoice merchantShortId opCity driverId serviceName req

sendSmsToDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> DDriver.SendSmsReq -> FlowHandler APISuccess
sendSmsToDriver merchantShortId opCity driverId volunteerId = withFlowHandlerAPI . DDriver.sendSmsToDriver merchantShortId opCity driverId volunteerId
