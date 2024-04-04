{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Search
  ( buildSearchReqV2,
  )
where

import qualified Beckn.OnDemand.Transformer.Search as Search
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601)
import qualified Domain.Action.UI.Search as DSearch
import Domain.Types.BecknConfig
import EulerHS.Prelude hiding (state, (%~))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Storage.CachedQueries.BecknConfig as QBC

buildSearchReqV2 ::
  (KvDbFlow m r, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DSearch.SearchRes ->
  m Spec.SearchReq
buildSearchReqV2 DSearch.SearchRes {..} = do
  bapUri <- Utils.mkBapUri merchant.id
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" AUTO_RICKSHAW >>= fromMaybeM (InternalError $ "Beckn Config not found for merchantId:-" <> show merchant.id.getId <> ",domain:-MOBILITY,vehicleVariant:-" <> show AUTO_RICKSHAW) -- get Vehicle Variatnt here
  ttl <- bapConfig.searchTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  Search.buildBecknSearchReqV2
    Context.SEARCH
    Context.MOBILITY
    origin
    stops
    searchId
    distance
    duration
    customerLanguage
    disabilityTag
    merchant
    bapUri
    city
    (getPoints shortestRouteInfo)
    phoneNumber
    isReallocationEnabled
    startTime
    multipleRoutes
    bapConfig
    ttl
  where
    getPoints val = val >>= (\routeInfo -> Just routeInfo.points)
