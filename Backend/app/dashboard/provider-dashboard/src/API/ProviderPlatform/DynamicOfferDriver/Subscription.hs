{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Subscription where

import qualified "dynamic-offer-driver-app" API.Dashboard.Subscription as SD
import Dashboard.Common as Common
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Payment as APayment
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Plan as DTPlan
import Domain.Types.AccessMatrix
import qualified "dynamic-offer-driver-app" Domain.Types.Invoice as INV
import "lib-dashboard" Domain.Types.Merchant as DMerchant
import qualified "dynamic-offer-driver-app" Domain.Types.Plan as DPlan
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "plan"
    :> ( ListPlan
           :<|> SelectPlan
           :<|> SuspendPlan
           :<|> SubscribePlan
           :<|> CurrentPlan
           :<|> PaymentStatus
       )

type ListPlan =
  ApiAuth 'DRIVER_OFFER_BPP 'SUBSCRIPTION 'LIST_PLAN
    :> SD.ListPlan

type SelectPlan =
  ApiAuth 'DRIVER_OFFER_BPP 'SUBSCRIPTION 'SELECT_PLAN
    :> SD.SelectPlan

type SuspendPlan =
  ApiAuth 'DRIVER_OFFER_BPP 'SUBSCRIPTION 'SUSPEND_PLAN
    :> SD.SuspendPlan

type SubscribePlan =
  ApiAuth 'DRIVER_OFFER_BPP 'SUBSCRIPTION 'SUBSCRIBE_PLAN
    :> SD.SubscribePlan

type CurrentPlan =
  ApiAuth 'DRIVER_OFFER_BPP 'SUBSCRIPTION 'CURRENT_PLAN
    :> SD.CurrentPlan

type PaymentStatus =
  ApiAuth 'DRIVER_OFFER_BPP 'SUBSCRIPTION 'PAYMENT_STATUS
    :> SD.OrderStatus

buildTransaction ::
  ( MonadFlow m
  ) =>
  SD.SubscriptionEndpoint ->
  ApiTokenInfo ->
  Maybe (Id Common.Driver) ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo mbDid =
  T.buildTransaction (DT.SubscriptionAPI endpoint) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) mbDid Nothing T.emptyRequest

handler :: ShortId DMerchant.Merchant -> FlowServer API
handler merchantId =
  planList merchantId
    :<|> planSelect merchantId
    :<|> planSuspend merchantId
    :<|> planSubscribe merchantId
    :<|> currentPlan merchantId
    :<|> paymentStatus merchantId

planList :: ShortId DMerchant.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler DTPlan.PlanListAPIRes
planList merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.subscription.planList) driverId

planSelect :: ShortId DMerchant.Merchant -> ApiTokenInfo -> Id Common.Driver -> Id DPlan.Plan -> FlowHandler APISuccess
planSelect merchantShortId apiTokenInfo driverId planId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction SD.SelectPlanEndpoint apiTokenInfo (Just driverId)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.subscription.planSelect) driverId planId

planSuspend :: ShortId DMerchant.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
planSuspend merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction SD.SuspendPlanEndpoint apiTokenInfo (Just driverId)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.subscription.planSuspend) driverId

planSubscribe :: ShortId DMerchant.Merchant -> ApiTokenInfo -> Id Common.Driver -> Id DPlan.Plan -> FlowHandler DTPlan.PlanSubscribeRes
planSubscribe merchantShortId apiTokenInfo driverId planId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction SD.SubscribePlanEndpoint apiTokenInfo (Just driverId)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.subscription.planSubscribe) driverId planId

currentPlan :: ShortId DMerchant.Merchant -> ApiTokenInfo -> Id Common.Driver -> FlowHandler DTPlan.CurrentPlanRes
currentPlan merchantShortId apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.subscription.currentPlan) driverId

paymentStatus :: ShortId DMerchant.Merchant -> ApiTokenInfo -> Id Common.Driver -> Id INV.Invoice -> FlowHandler APayment.PaymentStatusResp
paymentStatus merchantShortId apiTokenInfo driverId invoiceId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.subscription.paymentStatus) driverId invoiceId
