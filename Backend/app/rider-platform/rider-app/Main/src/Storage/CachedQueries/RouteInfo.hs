{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.RouteInfo where

import Domain.Action.UI.Search.OneWay as D
import Domain.Types.SearchRequest (SearchRequest)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.CachedQueries.CacheConfig

distanceAnndDurationInfo :: (Redis.HedisFlow m r, CacheFlow m r) => MonadFlow m => Id SearchRequest -> D.DistanceAndDuration -> m ()
distanceAnndDurationInfo searchId distanceAndDuration = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Redis.setExp (routeKey searchId) distanceAndDuration expTime

getdistanceAnndDurationInfo :: (Redis.HedisFlow m r) => Id SearchRequest -> m (Maybe D.DistanceAndDuration)
getdistanceAnndDurationInfo searchId = Redis.get @D.DistanceAndDuration (routeKey searchId)

routeKey :: Id SearchRequest -> Text
routeKey sreqId = "ParentSearchId:" <> sreqId.getId
