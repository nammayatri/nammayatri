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
import qualified API.Types.ProviderPlatform.Management.Booking as BookingDSL
import qualified API.Types.ProviderPlatform.Management.Driver as DriverDSL
import qualified API.Types.ProviderPlatform.Management.DriverCoins as DriverCoinsDSL
import qualified API.Types.ProviderPlatform.Management.DriverGoHome as DriverGoHomeDSL
import qualified API.Types.ProviderPlatform.Management.DriverReferral as DriverReferralDSL
import qualified API.Types.ProviderPlatform.Management.DriverRegistration as DriverRegistrationDSL
import qualified API.Types.ProviderPlatform.Management.Merchant as MerchantDSL
import qualified API.Types.ProviderPlatform.Management.Message as MessageDSL
import qualified API.Types.ProviderPlatform.Management.NammaTag as NammaTagDSL
import qualified API.Types.ProviderPlatform.Management.Payout as PayoutDSL
import qualified API.Types.ProviderPlatform.Management.Revenue as RevenueDSL
import qualified API.Types.ProviderPlatform.Management.Ride as RideDSL
import qualified API.Types.ProviderPlatform.Management.System as SystemDSL
import qualified Dashboard.ProviderPlatform.Management.Driver as Driver
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
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
import IssueManagement.Common
import qualified IssueManagement.Common.Dashboard.Issue as Issue
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueMessage
import IssueManagement.Domain.Types.Issue.IssueOption
import IssueManagement.Domain.Types.Issue.IssueReport
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Servant
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client

data DriverOperationAPIs = DriverOperationAPIs
  { subscription :: SubscriptionAPIs,
    overlay :: OverlayAPIs,
    issue :: IssueAPIs,
    merchantDSL :: MerchantDSL.MerchantAPIs,
    messageDSL :: MessageDSL.MessageAPIs,
    revenueDSL :: RevenueDSL.RevenueAPIs,
    rideDSL :: RideDSL.RideAPIs,
    nammaTagDSL :: NammaTagDSL.NammaTagAPIs,
    driverDSL :: DriverDSL.DriverAPIs,
    driverCoinsDSL :: DriverCoinsDSL.DriverCoinsAPIs,
    driverGoHomeDSL :: DriverGoHomeDSL.DriverGoHomeAPIs,
    driverReferralDSL :: DriverReferralDSL.DriverReferralAPIs,
    driverRegistrationDSL :: DriverRegistrationDSL.DriverRegistrationAPIs,
    bookingDSL :: BookingDSL.BookingAPIs,
    payoutDSL :: PayoutDSL.PayoutAPIs,
    systemDSL :: SystemDSL.SystemAPIs
  }

data OverlayAPIs = OverlayAPIs
  { createOverlay :: Overlay.CreateOverlayReq -> Euler.EulerClient APISuccess,
    deleteOverlay :: Overlay.DeleteOverlayReq -> Euler.EulerClient APISuccess,
    listOverlay :: Euler.EulerClient Overlay.ListOverlayResp,
    overlayInfo :: Text -> Maybe Text -> Euler.EulerClient Overlay.OverlayInfoResp,
    scheduleOverlay :: Overlay.ScheduleOverlay -> Euler.EulerClient APISuccess
  }

data IssueAPIs = IssueAPIs
  { issueCategoryList :: Euler.EulerClient Issue.IssueCategoryListRes,
    issueList :: Maybe Int -> Maybe Int -> Maybe IssueStatus -> Maybe (Id IssueCategory) -> Maybe Text -> Euler.EulerClient Issue.IssueReportListResponse,
    issueInfo :: Id IssueReport -> Euler.EulerClient Issue.IssueInfoRes,
    issueInfoV2 :: Maybe (Id IssueReport) -> Maybe (ShortId IssueReport) -> Euler.EulerClient Issue.IssueInfoRes,
    issueUpdate :: Id IssueReport -> Issue.IssueUpdateByUserReq -> Euler.EulerClient APISuccess,
    issueAddComment :: Id IssueReport -> Issue.IssueAddCommentByUserReq -> Euler.EulerClient APISuccess,
    issueFetchMedia :: Text -> Euler.EulerClient Text,
    ticketStatusCallBack :: A.Value -> Euler.EulerClient APISuccess,
    createIssueCategory :: Issue.CreateIssueCategoryReq -> Euler.EulerClient Issue.CreateIssueCategoryRes,
    updateIssueCategory :: Id IssueCategory -> Issue.UpdateIssueCategoryReq -> Euler.EulerClient APISuccess,
    createIssueOption :: Id IssueCategory -> Id IssueMessage -> Issue.CreateIssueOptionReq -> Euler.EulerClient Issue.CreateIssueOptionRes,
    updateIssueOption :: Id IssueOption -> Issue.UpdateIssueOptionReq -> Euler.EulerClient APISuccess,
    upsertIssueMessage :: (LBS.ByteString, Issue.UpsertIssueMessageReq) -> Euler.EulerClient Issue.UpsertIssueMessageRes
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
  let issue = IssueAPIs {..}
  let overlay = OverlayAPIs {..}

  let merchantDSL = MerchantDSL.mkMerchantAPIs merchantClientDSL
  let messageDSL = MessageDSL.mkMessageAPIs messageClientDSL
  let revenueDSL = RevenueDSL.mkRevenueAPIs revenueClientDSL
  let rideDSL = RideDSL.mkRideAPIs rideClientDSL
  let nammaTagDSL = NammaTagDSL.mkNammaTagAPIs nammaTagClientDSL
  let driverDSL = DriverDSL.mkDriverAPIs driverClientDSL
  let driverCoinsDSL = DriverCoinsDSL.mkDriverCoinsAPIs driverCoinsClientDSL
  let driverGoHomeDSL = DriverGoHomeDSL.mkDriverGoHomeAPIs driverGoHomeClientDSL
  let driverReferralDSL = DriverReferralDSL.mkDriverReferralAPIs driverReferralClientDSL
  let driverRegistrationDSL = DriverRegistrationDSL.mkDriverRegistrationAPIs driverRegistrationClientDSL
  let bookingDSL = BookingDSL.mkBookingAPIs bookingClientDSL
  let payoutDSL = PayoutDSL.mkPayoutAPIs payoutClientDSL
  let systemDSL = SystemDSL.mkSystemAPIs systemClientDSL
  DriverOperationAPIs {..}
  where
    subscriptionClient
      :<|> overlayClient
      :<|> issueClient
      :<|> merchantClientDSL
      :<|> messageClientDSL
      :<|> revenueClientDSL
      :<|> rideClientDSL
      :<|> nammaTagClientDSL
      :<|> driverClientDSL
      :<|> driverCoinsClientDSL
      :<|> driverGoHomeClientDSL
      :<|> driverReferralClientDSL
      :<|> driverRegistrationClientDSL
      :<|> bookingClientDSL
      :<|> payoutClientDSL
      :<|> systemClientDSL = clientWithMerchantAndCity (Proxy :: Proxy BPP.API) merchantId city token

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

    issueCategoryList
      :<|> issueList
      :<|> issueInfo
      :<|> issueInfoV2
      :<|> issueUpdate
      :<|> issueAddComment
      :<|> issueFetchMedia
      :<|> ticketStatusCallBack
      :<|> createIssueCategory
      :<|> updateIssueCategory
      :<|> createIssueOption
      :<|> updateIssueOption
      :<|> upsertIssueMessage = issueClient

callDriverOfferBPPOperations ::
  forall m r b c.
  CallServerAPI' DriverOperationAPIs m r b c =>
  CheckedShortId DM.Merchant ->
  City.City ->
  (DriverOperationAPIs -> b) ->
  c
callDriverOfferBPPOperations merchantId city = callServerAPI @_ @m @r DRIVER_OFFER_BPP_MANAGEMENT (mkDriverOperationAPIs merchantId city) "callDriverOfferBPPOperations"
