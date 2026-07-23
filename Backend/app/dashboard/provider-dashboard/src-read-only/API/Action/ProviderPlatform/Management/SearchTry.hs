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

type API = ("searchTry" :> PostSearchTryRecent)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postSearchTryRecent merchantId city

type PostSearchTryRecent =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.SEARCH_TRY) / ('API.Types.ProviderPlatform.Management.SearchTry.POST_SEARCH_TRY_RECENT))
      :> API.Types.ProviderPlatform.Management.SearchTry.PostSearchTryRecent
  )

postSearchTryRecent :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.SearchTry.RecentSearchTriesReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.SearchTry.RecentSearchTriesRes)
postSearchTryRecent merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.SearchTry.postSearchTryRecent merchantShortId opCity apiTokenInfo req
