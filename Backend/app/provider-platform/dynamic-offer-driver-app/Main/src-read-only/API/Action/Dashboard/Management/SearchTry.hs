{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.SearchTry
  ( API.Types.ProviderPlatform.Management.SearchTry.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.SearchTry
import qualified Domain.Action.Dashboard.Management.SearchTry
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.SearchTry.API)
handler merchantId city = postSearchTryRecent merchantId city

postSearchTryRecent :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.SearchTry.RecentSearchTriesReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.SearchTry.RecentSearchTriesRes)
postSearchTryRecent a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.SearchTry.postSearchTryRecent a3 a2 a1
