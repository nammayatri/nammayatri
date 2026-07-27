{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ProviderPlatformClient.DynamicOfferDriver
  ( callDriverOfferBppRecentSearchTries,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.SearchTry as ProviderSearch
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common hiding (callAPI)
import Servant
-- Client-side auth combinator only (same HeaderAuth "token" shape as driver-app).
-- Use rider-app's Tools.Auth to avoid depending on dynamic-offer-driver-app
-- (which collides on Domain.Action.Dashboard.* modules).
import qualified "rider-app" Tools.Auth as RiderAuth
import Tools.Client

-- Matches driver-app Management SearchTry endpoint:
-- POST /dashboard/:merchantId/:city/searchTry/recentSearchTries
type BPPRecentSearchTriesAPI =
  "dashboard"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> Capture "city" Context.City
    :> RiderAuth.DashboardTokenAuth
    :> "searchTry"
    :> "recentSearchTries"
    :> ReqBody '[JSON] ProviderSearch.RecentSearchTriesReq
    :> Post '[JSON] ProviderSearch.RecentSearchTriesRes

newtype BPPRecentSearchTriesAPIs = BPPRecentSearchTriesAPIs
  { postRecentSearchTries :: ProviderSearch.RecentSearchTriesReq -> Euler.EulerClient ProviderSearch.RecentSearchTriesRes
  }

mkBPPRecentSearchTriesAPIs :: ShortId DM.Merchant -> Context.City -> Text -> BPPRecentSearchTriesAPIs
mkBPPRecentSearchTriesAPIs merchantId city token =
  BPPRecentSearchTriesAPIs {..}
  where
    postRecentSearchTries = Euler.client (Proxy :: Proxy BPPRecentSearchTriesAPI) merchantId city token

callDriverOfferBppRecentSearchTries ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI BPPRecentSearchTriesAPIs m r b c
  ) =>
  ShortId DM.Merchant ->
  Context.City ->
  (BPPRecentSearchTriesAPIs -> b) ->
  c
callDriverOfferBppRecentSearchTries merchantId city =
  callServerAPI @_ @m @r DRIVER_OFFER_BPP_MANAGEMENT (mkBPPRecentSearchTriesAPIs merchantId city) "callDriverOfferBppRecentSearchTries"
