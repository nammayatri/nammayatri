{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Fleet.PayoutAccount
  ( API.Types.ProviderPlatform.Fleet.PayoutAccount.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Fleet.PayoutAccount.API)
handler merchantId city = postPayoutAccount merchantId city

postPayoutAccount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountResp)
postPayoutAccount a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.PayoutAccount.postPayoutAccount a4 a3 a2 a1
