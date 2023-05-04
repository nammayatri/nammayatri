{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.RouteInfo where

import Domain.Types.SearchRequest (SearchRequest)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.CachedQueries.CacheConfig
import qualified Tools.Maps as Maps

cacheRouteInfo :: (Redis.HedisFlow m r, CacheFlow m r) => MonadFlow m => Id SearchRequest -> [Maps.RouteInfo] -> m ()
cacheRouteInfo searchId routeInfo = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Redis.setExp (routeKey searchId) routeInfo expTime

getRouteInfo :: (Redis.HedisFlow m r) => Id SearchRequest -> m (Maybe [Maps.RouteInfo])
getRouteInfo searchId = Redis.get (routeKey searchId)

routeKey :: Id SearchRequest -> Text
routeKey sreqId = "ParentSearchId:" <> sreqId.getId
