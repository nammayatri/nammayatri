{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Route
  ( Maps.GetRoutesReq,
    Maps.GetRoutesResp,
    getRoutes,
    getPickupRoutes,
    getTripRoutes,
  )
where

import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DP
import Kernel.Storage.Hedis (CacheFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Tools.Maps as Maps
import Tools.Metrics (CoreMetrics)

getRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => (Id DP.Person, Id Merchant.Merchant) -> Maps.GetRoutesReq -> m Maps.GetRoutesResp
getRoutes (_, merchantId) req = do
  Maps.getRoutes merchantId req

getPickupRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => (Id DP.Person, Id Merchant.Merchant) -> Maps.GetRoutesReq -> m Maps.GetRoutesResp
getPickupRoutes (_, merchantId) req = do
  Maps.getPickupRoutes merchantId req

getTripRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => (Id DP.Person, Id Merchant.Merchant) -> Maps.GetRoutesReq -> m Maps.GetRoutesResp
getTripRoutes (_, merchantId) req = do
  Maps.getTripRoutes merchantId req
