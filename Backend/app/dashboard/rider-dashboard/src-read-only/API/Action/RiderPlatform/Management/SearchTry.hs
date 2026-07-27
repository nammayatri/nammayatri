{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.SearchTry
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.SearchTry
import qualified Domain.Action.RiderPlatform.Management.SearchTry
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("searchTry" :> PostSearchTryRecentSearchTries)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postSearchTryRecentSearchTries merchantId city

type PostSearchTryRecentSearchTries =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.SEARCH_TRY) / ('API.Types.RiderPlatform.Management.SearchTry.POST_SEARCH_TRY_RECENT_SEARCH_TRIES))
      :> API.Types.RiderPlatform.Management.SearchTry.PostSearchTryRecentSearchTries
  )

postSearchTryRecentSearchTries :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.SearchTry.RecentSearchTriesReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.SearchTry.RecentSearchTriesRes)
postSearchTryRecentSearchTries merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.SearchTry.postSearchTryRecentSearchTries merchantShortId opCity apiTokenInfo req
