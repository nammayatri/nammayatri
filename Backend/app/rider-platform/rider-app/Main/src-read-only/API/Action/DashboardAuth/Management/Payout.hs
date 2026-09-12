{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Payout
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Payout
import qualified Domain.Action.Dashboard.Payout
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.API.Payout.Types
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("payout" :> GetPayoutPayoutOrder)

type GetPayoutPayoutOrder = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT_ORDER" :> API.Types.RiderPlatform.Management.Payout.GetPayoutPayoutOrder)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPayoutPayoutOrder merchantId city

getPayoutPayoutOrder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutOrderResp)
getPayoutPayoutOrder a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Payout.getPayoutPayoutOrder a4 a3 a1
