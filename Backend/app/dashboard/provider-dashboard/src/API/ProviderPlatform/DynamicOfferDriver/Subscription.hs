{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Subscription where

import qualified "dynamic-offer-driver-app" API.Dashboard.Management.Subscription as SD
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
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations as Client
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
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'SUBSCRIPTION 'LIST_PLAN
    :> SD.ListPlan

type SelectPlan =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'SUBSCRIPTION 'SELECT_PLAN
    :> SD.SelectPlan

type SuspendPlan =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'SUBSCRIPTION 'SUSPEND_PLAN
    :> SD.SuspendPlan

type SubscribePlan =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'SUBSCRIPTION 'SUBSCRIBE_PLAN
    :> SD.SubscribePlan

type CurrentPlan =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'SUBSCRIPTION 'CURRENT_PLAN
    :> SD.CurrentPlan

type PaymentStatus =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'SUBSCRIPTION 'PAYMENT_STATUS
    :> SD.OrderStatus

buildTransaction ::
  ( MonadFlow m
  ) =>
  SD.SubscriptionEndpoint ->
  ApiTokenInfo ->
  Maybe (Id Common.Driver) ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo mbDid =
  T.buildTransaction (DT.SubscriptionAPI endpoint) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) mbDid Nothing T.emptyRequest

handler :: ShortId DMerchant.Merchant -> City.City -> FlowServer API
handler merchantId city =
  planList merchantId city
    :<|> planSelect merchantId city
    :<|> planSuspend merchantId city
    :<|> planSubscribe merchantId city
    :<|> currentPlan merchantId city
    :<|> paymentStatus merchantId city

planList :: ShortId DMerchant.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler DTPlan.PlanListAPIRes
planList merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.subscription.planList) driverId

planSelect :: ShortId DMerchant.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Id DPlan.Plan -> FlowHandler APISuccess
planSelect merchantShortId opCity apiTokenInfo driverId planId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction SD.SelectPlanEndpoint apiTokenInfo (Just driverId)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.subscription.planSelect) driverId planId

planSuspend :: ShortId DMerchant.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler APISuccess
planSuspend merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction SD.SuspendPlanEndpoint apiTokenInfo (Just driverId)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.subscription.planSuspend) driverId

planSubscribe :: ShortId DMerchant.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Id DPlan.Plan -> FlowHandler DTPlan.PlanSubscribeRes
planSubscribe merchantShortId opCity apiTokenInfo driverId planId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction SD.SubscribePlanEndpoint apiTokenInfo (Just driverId)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.subscription.planSubscribe) driverId planId

currentPlan :: ShortId DMerchant.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> FlowHandler DTPlan.CurrentPlanRes
currentPlan merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.subscription.currentPlan) driverId

paymentStatus :: ShortId DMerchant.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Id INV.Invoice -> FlowHandler APayment.PaymentStatusResp
paymentStatus merchantShortId opCity apiTokenInfo driverId invoiceId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.subscription.paymentStatus) driverId invoiceId
