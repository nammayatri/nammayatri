{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.SearchTry
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.SearchTry
import qualified Domain.Action.Dashboard.SearchTry
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("searchTry" :> PostSearchTryRecentSearchTries)

type PostSearchTryRecentSearchTries =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/SEARCH_TRY/POST_SEARCH_TRY_RECENT_SEARCH_TRIES"
      :> API.Types.RiderPlatform.Management.SearchTry.PostSearchTryRecentSearchTries
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postSearchTryRecentSearchTries merchantId city

postSearchTryRecentSearchTries :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.SearchTry.RecentSearchTriesReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.SearchTry.RecentSearchTriesRes)
postSearchTryRecentSearchTries a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.SearchTry.postSearchTryRecentSearchTries a4 a3 a1
