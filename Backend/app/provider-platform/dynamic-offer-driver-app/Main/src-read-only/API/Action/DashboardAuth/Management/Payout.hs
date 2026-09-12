{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Payout
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Payout
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.Payout
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.API.Payout.Types
import qualified "payment" Lib.Payment.Domain.Types.PayoutRequest
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("payout" :> (GetPayoutPayoutHistory :<|> GetPayoutPayoutReferralHistory :<|> GetPayoutPayoutOrder :<|> GetPayoutPayout :<|> PostPayoutPayoutRetry :<|> PostPayoutPayoutCancel :<|> PostPayoutPayoutCash :<|> PostPayoutPayoutVpaDelete :<|> PostPayoutPayoutVpaUpdate :<|> PostPayoutPayoutVpaRefundRegistration :<|> PostPayoutPayoutScheduledPayoutConfigUpsert))

type GetPayoutPayoutHistory =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT_HISTORY"
      :> API.Types.ProviderPlatform.Management.Payout.GetPayoutPayoutHistory
  )

type GetPayoutPayoutReferralHistory =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT_REFERRAL_HISTORY"
      :> API.Types.ProviderPlatform.Management.Payout.GetPayoutPayoutReferralHistory
  )

type GetPayoutPayoutOrder = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT_ORDER" :> API.Types.ProviderPlatform.Management.Payout.GetPayoutPayoutOrder)

type GetPayoutPayout = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT" :> API.Types.ProviderPlatform.Management.Payout.GetPayoutPayout)

type PostPayoutPayoutRetry =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_RETRY"
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutRetry
  )

type PostPayoutPayoutCancel =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_CANCEL"
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutCancel
  )

type PostPayoutPayoutCash =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_CASH"
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutCash
  )

type PostPayoutPayoutVpaDelete =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_VPA_DELETE"
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutVpaDelete
  )

type PostPayoutPayoutVpaUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_VPA_UPDATE"
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutVpaUpdate
  )

type PostPayoutPayoutVpaRefundRegistration =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_VPA_REFUND_REGISTRATION"
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutVpaRefundRegistration
  )

type PostPayoutPayoutScheduledPayoutConfigUpsert =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_SCHEDULED_PAYOUT_CONFIG_UPSERT"
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutScheduledPayoutConfigUpsert
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPayoutPayoutHistory merchantId city :<|> getPayoutPayoutReferralHistory merchantId city :<|> getPayoutPayoutOrder merchantId city :<|> getPayoutPayout merchantId city :<|> postPayoutPayoutRetry merchantId city :<|> postPayoutPayoutCancel merchantId city :<|> postPayoutPayoutCash merchantId city :<|> postPayoutPayoutVpaDelete merchantId city :<|> postPayoutPayoutVpaUpdate merchantId city :<|> postPayoutPayoutVpaRefundRegistration merchantId city :<|> postPayoutPayoutScheduledPayoutConfigUpsert merchantId city

getPayoutPayoutHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutHistoryRes)
getPayoutPayoutHistory a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.getPayoutPayoutHistory a10 a9 a7 a6 a5 a4 a3 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a8))

getPayoutPayoutReferralHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe ((Kernel.Types.Id.Id Dashboard.Common.Driver)) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Payout.PayoutReferralHistoryRes)
getPayoutPayoutReferralHistory a12 a11 _a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.getPayoutPayoutReferralHistory a12 a11 a9 a8 a7 a6 a5 a4 a3 a2 a1

getPayoutPayoutOrder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutOrderResp)
getPayoutPayoutOrder a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.getPayoutPayoutOrder a4 a3 a1

getPayoutPayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutRequestResp)
getPayoutPayout a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.getPayoutPayout a4 a3 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2))

postPayoutPayoutRetry :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutRetry a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutRetry a4 a3 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2))

postPayoutPayoutCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Lib.Payment.API.Payout.Types.PayoutCancelReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutCancel a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutCancel a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1

postPayoutPayoutCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Lib.Payment.API.Payout.Types.PayoutCashUpdateReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutCash a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutCash a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1

postPayoutPayoutVpaDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Payment.API.Payout.Types.DeleteVpaReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutVpaDelete a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutVpaDelete a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1

postPayoutPayoutVpaUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Payment.API.Payout.Types.UpdateVpaReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutVpaUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutVpaUpdate a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1

postPayoutPayoutVpaRefundRegistration :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Payment.API.Payout.Types.RefundRegAmountReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutVpaRefundRegistration a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutVpaRefundRegistration a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1

postPayoutPayoutScheduledPayoutConfigUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Payout.UpdateScheduledPayoutConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutScheduledPayoutConfigUpsert a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutScheduledPayoutConfigUpsert a4 a3 a1
