{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.CoinsConfig
  ( API.Types.ProviderPlatform.Management.CoinsConfig.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.CoinsConfig
import qualified Domain.Action.Dashboard.Management.CoinsConfig
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.CoinsConfig.API)
handler merchantId city = putCoinsConfigUpdate merchantId city :<|> postCoinsConfigCreate merchantId city

putCoinsConfigUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.CoinsConfig.UpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putCoinsConfigUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.CoinsConfig.putCoinsConfigUpdate a3 a2 a1

postCoinsConfigCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.CoinsConfig.CreateCoinsConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCoinsConfigCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.CoinsConfig.postCoinsConfigCreate a3 a2 a1
