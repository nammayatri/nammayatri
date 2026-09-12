{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Fleet.PayoutAccount
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.PayoutAccount
import qualified Domain.Action.Dashboard.Fleet.PayoutAccount
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("fleet" :> (PostPayoutAccount :<|> PostPayoutAccountStatus))

type PostPayoutAccount =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/PAYOUT_ACCOUNT/POST_PAYOUT_ACCOUNT"
      :> API.Types.ProviderPlatform.Fleet.PayoutAccount.PostPayoutAccount
  )

type PostPayoutAccountStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/PAYOUT_ACCOUNT/POST_PAYOUT_ACCOUNT_STATUS"
      :> API.Types.ProviderPlatform.Fleet.PayoutAccount.PostPayoutAccountStatus
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postPayoutAccount merchantId city :<|> postPayoutAccountStatus merchantId city

postPayoutAccount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountResp)
postPayoutAccount a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.PayoutAccount.postPayoutAccount a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

postPayoutAccountStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountStatusReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountStatusResp)
postPayoutAccountStatus a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.PayoutAccount.postPayoutAccountStatus a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
