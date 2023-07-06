{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Domain.Action.UI.Route
  ( SDC.GetRoutesReq,
    SDC.GetRoutesResp,
    getRoutes,
    getPickupRoutes,
    getTripRoutes,
  )
where

import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBEnv)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DirectionsCache as SDC
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Tools.Maps as Maps
import Tools.Metrics (CoreMetrics)

getRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m, HasField "esqDBReplicaEnv" r EsqDBEnv) => (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> SDC.GetRoutesReq -> m SDC.GetRoutesResp
getRoutes (_, merchantId, merchantOperatingCityId) = SDC.getRoutes merchantId merchantOperatingCityId

getPickupRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maps.GetRoutesReq -> m Maps.GetRoutesResp
getPickupRoutes (_, _, merchantOperatingCityId) req = do
  Maps.getPickupRoutes merchantOperatingCityId req

getTripRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maps.GetRoutesReq -> m Maps.GetRoutesResp
getTripRoutes (_, _, merchantOperatingCityId) req = do
  Maps.getTripRoutes merchantOperatingCityId req
