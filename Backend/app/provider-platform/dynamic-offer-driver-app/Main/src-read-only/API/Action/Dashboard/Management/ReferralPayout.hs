{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.ReferralPayout
  ( API.Types.ProviderPlatform.Management.ReferralPayout.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.ReferralPayout
import qualified Dashboard.Common
import qualified Data.Text
import qualified Domain.Action.Dashboard.Management.ReferralPayout
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Payout.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.ReferralPayout.API)
handler merchantId city = postReferralPayoutDashboardPayoutDeleteVpa merchantId city :<|> getReferralPayoutDashboardPayoutRegistration merchantId city :<|> getReferralPayoutDashboardPayoutOrderStatus merchantId city

postReferralPayoutDashboardPayoutDeleteVpa :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver)) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postReferralPayoutDashboardPayoutDeleteVpa a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ReferralPayout.postReferralPayoutDashboardPayoutDeleteVpa a3 a2 a1

getReferralPayoutDashboardPayoutRegistration :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver)) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.ReferralPayout.ReferralPayoutRegistrationRes)
getReferralPayoutDashboardPayoutRegistration a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ReferralPayout.getReferralPayoutDashboardPayoutRegistration a3 a2 a1

getReferralPayoutDashboardPayoutOrderStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver)) -> Data.Text.Text -> Environment.FlowHandler Kernel.External.Payout.Interface.Types.PayoutOrderStatusResp)
getReferralPayoutDashboardPayoutOrderStatus a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.ReferralPayout.getReferralPayoutDashboardPayoutOrderStatus a4 a3 a2 a1
