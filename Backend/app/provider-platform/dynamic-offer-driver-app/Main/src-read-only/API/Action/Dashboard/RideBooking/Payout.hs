{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking.Payout
  ( API.Types.Dashboard.RideBooking.Payout.API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Payout
import qualified Domain.Action.Dashboard.RideBooking.Payout
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.Payout.API)
handler merchantId city = getPayoutStatus merchantId city :<|> postPayoutCancel merchantId city :<|> postPayoutRetry merchantId city :<|> postPayoutMarkCashPaid merchantId city

getPayoutStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.RideBooking.Payout.PayoutStatusResp)
getPayoutStatus a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Payout.getPayoutStatus a3 a2 a1

postPayoutCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.Dashboard.RideBooking.Payout.PayoutCancelReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutCancel a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Payout.postPayoutCancel a4 a3 a2 a1

postPayoutRetry :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutRetry a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Payout.postPayoutRetry a3 a2 a1

postPayoutMarkCashPaid :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutMarkCashPaid a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Payout.postPayoutMarkCashPaid a4 a3 a2 a1
