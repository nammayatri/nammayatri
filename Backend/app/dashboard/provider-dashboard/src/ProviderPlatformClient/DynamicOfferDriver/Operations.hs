{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ProviderPlatformClient.DynamicOfferDriver.Operations
  ( callDriverOfferBPPOperations,
  )
where

import "dynamic-offer-driver-app" API.Dashboard.Management as BPP
import qualified API.Dashboard.Management.Subscription as MSubscription
import qualified Dashboard.ProviderPlatform.Management.Driver as Driver
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Driver as DDriver
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Overlay as Overlay
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Driver as ADriver
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Payment as APayment
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Plan as Subscription
import qualified "dynamic-offer-driver-app" Domain.Types.Invoice as INV
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "dynamic-offer-driver-app" Domain.Types.Plan as DPlan
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Servant
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client

data DriverOperationAPIs = DriverOperationAPIs
  { subscription :: SubscriptionAPIs,
    overlay :: OverlayAPIs
  }

data OverlayAPIs = OverlayAPIs
  { createOverlay :: Overlay.CreateOverlayReq -> Euler.EulerClient APISuccess,
    deleteOverlay :: Overlay.DeleteOverlayReq -> Euler.EulerClient APISuccess,
    listOverlay :: Euler.EulerClient Overlay.ListOverlayResp,
    overlayInfo :: Text -> Maybe Text -> Euler.EulerClient Overlay.OverlayInfoResp,
    scheduleOverlay :: Overlay.ScheduleOverlay -> Euler.EulerClient APISuccess
  }

data SubscriptionAPIs = SubscriptionAPIs
  { planListV2 :: Id Driver.Driver -> DPlan.ServiceNames -> Euler.EulerClient Subscription.PlanListAPIRes,
    planSelectV2 :: Id Driver.Driver -> Id DPlan.Plan -> DPlan.ServiceNames -> Euler.EulerClient APISuccess,
    planSuspendV2 :: Id Driver.Driver -> DPlan.ServiceNames -> Euler.EulerClient APISuccess,
    planSubscribeV2 :: Id Driver.Driver -> Id DPlan.Plan -> DPlan.ServiceNames -> MSubscription.PlanSubscribeReq -> Euler.EulerClient Subscription.PlanSubscribeRes,
    currentPlanV2 :: Id Driver.Driver -> DPlan.ServiceNames -> Euler.EulerClient Subscription.CurrentPlanRes,
    paymentStatus :: Id Driver.Driver -> Id INV.Invoice -> Euler.EulerClient APayment.PaymentStatusResp,
    getPaymentHistoryV2 :: Id Driver.Driver -> DPlan.ServiceNames -> Maybe INV.InvoicePaymentMode -> Maybe Int -> Maybe Int -> Euler.EulerClient ADriver.HistoryEntityV2,
    getPaymentHistoryEntityDetailsV2 :: Id Driver.Driver -> DPlan.ServiceNames -> Id INV.Invoice -> Euler.EulerClient ADriver.HistoryEntryDetailsEntityV2,
    updateSubscriptionDriverFeeAndInvoice :: Id Driver.Driver -> Driver.ServiceNames -> Driver.SubscriptionDriverFeesAndInvoicesToUpdate -> Euler.EulerClient Driver.SubscriptionDriverFeesAndInvoicesToUpdate,
    sendMessageToDriverViaDashboard :: Id Driver.Driver -> Text -> DDriver.SendSmsReq -> Euler.EulerClient APISuccess,
    planList :: Id Driver.Driver -> Euler.EulerClient Subscription.PlanListAPIRes,
    planSelect :: Id Driver.Driver -> Id DPlan.Plan -> Euler.EulerClient APISuccess,
    planSuspend :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    planSubscribe :: Id Driver.Driver -> Id DPlan.Plan -> Euler.EulerClient Subscription.PlanSubscribeRes,
    currentPlan :: Id Driver.Driver -> Euler.EulerClient Subscription.CurrentPlanRes,
    getPaymentHistory :: Id Driver.Driver -> Maybe INV.InvoicePaymentMode -> Maybe Int -> Maybe Int -> Euler.EulerClient ADriver.HistoryEntityV2,
    getPaymentHistoryEntityDetails :: Id Driver.Driver -> Id INV.Invoice -> Euler.EulerClient ADriver.HistoryEntryDetailsEntityV2,
    collectManualPayments :: Id Driver.Driver -> DPlan.ServiceNames -> MSubscription.CollectManualPaymentsReq -> Euler.EulerClient APISuccess
  }

mkDriverOperationAPIs :: CheckedShortId DM.Merchant -> City.City -> Text -> DriverOperationAPIs
mkDriverOperationAPIs merchantId city token = do
  let subscription = SubscriptionAPIs {..}
  let overlay = OverlayAPIs {..}
  DriverOperationAPIs {..}
  where
    subscriptionClient
      :<|> overlayClient = clientWithMerchantAndCity (Proxy :: Proxy BPP.API) merchantId city token

    planListV2
      :<|> planSelectV2
      :<|> planSuspendV2
      :<|> planSubscribeV2
      :<|> currentPlanV2
      :<|> planList
      :<|> planSelect
      :<|> planSuspend
      :<|> planSubscribe
      :<|> currentPlan
      :<|> paymentStatus
      :<|> getPaymentHistoryV2
      :<|> getPaymentHistoryEntityDetailsV2
      :<|> getPaymentHistory
      :<|> getPaymentHistoryEntityDetails
      :<|> updateSubscriptionDriverFeeAndInvoice
      :<|> sendMessageToDriverViaDashboard
      :<|> collectManualPayments = subscriptionClient

    createOverlay
      :<|> deleteOverlay
      :<|> listOverlay
      :<|> overlayInfo
      :<|> scheduleOverlay = overlayClient

callDriverOfferBPPOperations ::
  forall m r b c.
  CallServerAPI' DriverOperationAPIs m r b c =>
  CheckedShortId DM.Merchant ->
  City.City ->
  (DriverOperationAPIs -> b) ->
  c
callDriverOfferBPPOperations merchantId city = callServerAPI @_ @m @r DRIVER_OFFER_BPP_MANAGEMENT (mkDriverOperationAPIs merchantId city) "callDriverOfferBPPOperations"
