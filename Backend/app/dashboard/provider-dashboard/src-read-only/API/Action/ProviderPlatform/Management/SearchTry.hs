{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.SearchTry
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.SearchTry
import qualified Domain.Action.ProviderPlatform.Management.SearchTry
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("searchTry" :> PostSearchTryRecentSearchTries)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postSearchTryRecentSearchTries merchantId city

type PostSearchTryRecentSearchTries =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.SEARCH_TRY / 'API.Types.ProviderPlatform.Management.SearchTry.POST_SEARCH_TRY_RECENT_SEARCH_TRIES)
      :> API.Types.ProviderPlatform.Management.SearchTry.PostSearchTryRecentSearchTries
  )

postSearchTryRecentSearchTries :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.SearchTry.RecentSearchTriesReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.SearchTry.RecentSearchTriesRes)
postSearchTryRecentSearchTries merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.SearchTry.postSearchTryRecentSearchTries merchantShortId opCity apiTokenInfo req
