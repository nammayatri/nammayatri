module Storage.CachedQueries.Maps.LocationMapCache where

import Domain.Types.HotSpot
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Storage.CachedQueries.CacheConfig

makeHotSpotKey :: Text
makeHotSpotKey = "CachedQueries:HotSpot"

getLocationsInCache :: CacheFlow m r => m [HotSpot]
getLocationsInCache =
  Hedis.safeGet makeHotSpotKey >>= \case
    Just a -> return a
    Nothing -> return []

setLocationInCache :: CacheFlow m r => [HotSpot] -> m ()
setLocationInCache hotspots = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeHotSpotKey
  Hedis.setExp key hotspots expTime
