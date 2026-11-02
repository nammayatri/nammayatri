{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Payout
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Payout
import qualified Control.Lens
import qualified Domain.Action.Dashboard.Payout
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.API.Payout.Types
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("payout" :> (GetPayoutPayoutOrder :<|> GetPayoutTransferNeedsReview :<|> PostPayoutTransferResolve))

type GetPayoutPayoutOrder = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT_ORDER" :> API.Types.RiderPlatform.Management.Payout.GetPayoutPayoutOrder)

type GetPayoutTransferNeedsReview =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_TRANSFER_NEEDS_REVIEW"
      :> API.Types.RiderPlatform.Management.Payout.GetPayoutTransferNeedsReview
  )

type PostPayoutTransferResolve =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_TRANSFER_RESOLVE"
      :> API.Types.RiderPlatform.Management.Payout.PostPayoutTransferResolve
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPayoutPayoutOrder merchantId city :<|> getPayoutTransferNeedsReview merchantId city :<|> postPayoutTransferResolve merchantId city

getPayoutPayoutOrder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutOrderResp)
getPayoutPayoutOrder a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Payout.getPayoutPayoutOrder a4 a3 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2))

getPayoutTransferNeedsReview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.RiderPlatform.Management.Payout.TransferReviewListRes)
getPayoutTransferNeedsReview a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Payout.getPayoutTransferNeedsReview a5 a4 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3))

postPayoutTransferResolve :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.RiderPlatform.Management.Payout.TransferResolveReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Payout.TransferResolveRes)
postPayoutTransferResolve a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_TRANSFER_RESOLVE" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Payout.postPayoutTransferResolve a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )
