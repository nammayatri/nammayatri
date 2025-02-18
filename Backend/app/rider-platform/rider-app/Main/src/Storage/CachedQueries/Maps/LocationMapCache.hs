{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.Maps.LocationMapCache where

import Domain.Types.HotSpot
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common (CacheFlow)

makeHotSpotKey :: Text -> Text
makeHotSpotKey geohash = "CachedQueries:HotSpot:" <> geohash

makeHotSpotKey' :: Text
makeHotSpotKey' = "CachedQueries:HotSpot"

getLocationsInCache :: CacheFlow m r => m [HotSpot]
getLocationsInCache =
  Hedis.safeGet makeHotSpotKey' >>= \case
    Just a -> return a
    Nothing -> return []

setLocationInCache :: CacheFlow m r => [HotSpot] -> m ()
setLocationInCache hotspots = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeHotSpotKey'
  Hedis.setExp key hotspots expTime
