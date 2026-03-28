{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Toll
  ( API.Types.ProviderPlatform.Management.Toll.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Toll
import qualified Domain.Action.Dashboard.Management.Toll
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Toll.API)
handler merchantId city = getTollList merchantId city :<|> putTollUpdate merchantId city

getTollList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Toll.TollRes])
getTollList a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Toll.getTollList a2 a1

putTollUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Toll.UpdateTollReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putTollUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Toll.putTollUpdate a3 a2 a1
