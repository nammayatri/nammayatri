{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Management.Subscription where

import qualified API.UI.Plan as DPlan
import qualified "dashboard-helper-api" Dashboard.Common as DP
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Action.UI.Driver as Driver
import qualified Domain.Action.UI.Payment as APayment
import qualified Domain.Action.UI.Plan as DTPlan
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Merchant as DM
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
  deriving (Show, Read)

derivePersistField "SubscriptionEndpoint"

type API =
  "plan"
    :> ( ListPlan
           :<|> SelectPlan
           :<|> SuspendPlan
           :<|> SubscribePlan
           :<|> CurrentPlan
           :<|> OrderStatus
           :<|> DriverPaymentHistoryAPI
           :<|> DriverPaymentHistoryEntityDetailsAPI
           :<|> Common.UpdateSubscriptionDriverFeeAndInvoiceAPI
           :<|> SendSmsToDriverAPI
       )

type ListPlan =
  Capture "driverId" (Id DP.Driver)
    :> "list"
    :> Get '[JSON] DTPlan.PlanListAPIRes

type SelectPlan =
  Capture "driverId" (Id DP.Driver)
    :> Capture "planId" (Id DPlan.Plan)
    :> "select"
    :> Put '[JSON] APISuccess

type SuspendPlan =
  Capture "driverId" (Id DP.Driver)
    :> "suspend"
    :> Put '[JSON] APISuccess

type SubscribePlan =
  Capture "driverId" (Id DP.Driver)
    :> Capture "planId" (Id DPlan.Plan)
    :> "subscribe"
    :> Post '[JSON] DTPlan.PlanSubscribeRes

type CurrentPlan =
  Capture "driverId" (Id DP.Driver)
    :> Get '[JSON] DTPlan.CurrentPlanRes

type OrderStatus =
  Capture "driverId" (Id DP.Driver)
    :> Capture "orderId" (Id INV.Invoice)
    :> "status"
    :> Get '[JSON] APayment.PaymentStatusResp

----- payment history ----------
type DriverPaymentHistoryAPI =
  Capture "driverId" (Id Common.Driver)
    :> "payments"
    :> "history"
    :> QueryParam "paymentMode" INV.InvoicePaymentMode
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] Driver.HistoryEntityV2

----------- payment history entry  -------------
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
  planList merchantId city
    :<|> planSelect merchantId city
    :<|> planSuspend merchantId city
    :<|> planSubscribe merchantId city
    :<|> currentPlan merchantId city
    :<|> paymentStatus merchantId city
    :<|> getPaymentHistory merchantId city
    :<|> getPaymentHistoryEntityDetails merchantId city
    :<|> updateDriverSubscriptionDriverFeeAndInvoiceUpdate merchantId city
    :<|> sendSmsToDriver merchantId city

planList :: ShortId DM.Merchant -> Context.City -> Id DP.Driver -> FlowHandler DTPlan.PlanListAPIRes
planList merchantShortId opCity driverId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  mOCityId <- withFlowHandlerAPI $ CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  DPlan.planList (cast driverId, m.id, mOCityId) (Just 0) (Just 50)

planSelect :: ShortId DM.Merchant -> Context.City -> Id DP.Driver -> Id DPlan.Plan -> FlowHandler APISuccess
planSelect merchantShortId opCity driverId planId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  mOCityId <- withFlowHandlerAPI $ CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  DPlan.planSelect planId (cast driverId, m.id, mOCityId)

planSuspend :: ShortId DM.Merchant -> Context.City -> Id DP.Driver -> FlowHandler APISuccess
planSuspend merchantShortId opCity driverId = withFlowHandlerAPI $ do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  DTPlan.planSuspend True (cast driverId, m.id, mOCityId)

planSubscribe :: ShortId DM.Merchant -> Context.City -> Id DP.Driver -> Id DPlan.Plan -> FlowHandler DTPlan.PlanSubscribeRes
planSubscribe merchantShortId opCity driverId planId = withFlowHandlerAPI $ do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  driverInfo <- DI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  DTPlan.planSubscribe planId True (cast driverId, m.id, mOCityId) driverInfo

currentPlan :: ShortId DM.Merchant -> Context.City -> Id DP.Driver -> FlowHandler DTPlan.CurrentPlanRes
currentPlan merchantShortId opCity driverId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  mOCityId <- withFlowHandlerAPI $ CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  DPlan.currentPlan (cast driverId, m.id, mOCityId)

paymentStatus :: ShortId DM.Merchant -> Context.City -> Id DP.Driver -> Id INV.Invoice -> FlowHandler APayment.PaymentStatusResp
paymentStatus merchantShortId opCity driverId invoiceId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  moCityId <- withFlowHandlerAPI $ CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  withFlowHandlerAPI $ APayment.getStatus (cast driverId, m.id, moCityId) (cast invoiceId)

getPaymentHistory :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Maybe INV.InvoicePaymentMode -> Maybe Int -> Maybe Int -> FlowHandler Driver.HistoryEntityV2
getPaymentHistory merchantShortId opCity driverId invoicePaymentMode limit offset = withFlowHandlerAPI $ DDriver.getPaymentHistory merchantShortId opCity driverId invoicePaymentMode limit offset

getPaymentHistoryEntityDetails :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Id INV.Invoice -> FlowHandler Driver.HistoryEntryDetailsEntityV2
getPaymentHistoryEntityDetails merchantShortId opCity driverId invoiceId = do
  withFlowHandlerAPI $ DDriver.getPaymentHistoryEntityDetails merchantShortId opCity driverId invoiceId

updateDriverSubscriptionDriverFeeAndInvoiceUpdate :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.SubscriptionDriverFeesAndInvoicesToUpdate -> FlowHandler Common.SubscriptionDriverFeesAndInvoicesToUpdate
updateDriverSubscriptionDriverFeeAndInvoiceUpdate merchantShortId opCity driverId req = withFlowHandlerAPI $ DDriver.updateSubscriptionDriverFeeAndInvoice merchantShortId opCity driverId req

sendSmsToDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> DDriver.SendSmsReq -> FlowHandler APISuccess
sendSmsToDriver merchantShortId opCity driverId volunteerId = withFlowHandlerAPI . DDriver.sendSmsToDriver merchantShortId opCity driverId volunteerId
