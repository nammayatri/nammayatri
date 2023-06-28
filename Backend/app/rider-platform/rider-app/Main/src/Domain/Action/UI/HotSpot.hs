module Domain.Action.UI.HotSpot where

import qualified Data.List as List
import Domain.Types.HotSpot
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import Storage.CachedQueries.Maps.LocationMapCache
import Tools.Metrics

frequencyUpdator ::
  ( HasCacheConfig r,
    CoreMetrics m,
    HedisFlow m r,
    HasFlowEnv m r '["hotSpotGeoHashPrecision" ::: Int]
  ) =>
  LatLong ->
  m ()
frequencyUpdator latLong = do
  previousHotSpots <- getLocationsInCache
  HotSpot {..} <- convertToHotSpot latLong
  let mbTargetHotSpot = List.find (\hSpot -> hSpot.geoHash == geoHash) previousHotSpots
  case mbTargetHotSpot of
    Just foundHotSpot -> do
      let otherHotSpot = List.filter (\hSpot -> hSpot.geoHash /= foundHotSpot.geoHash) previousHotSpots
      let allHotSpots = HotSpot {frequency = foundHotSpot.frequency + 1, ..} : otherHotSpot
      setLocationInCache allHotSpots
    Nothing -> do
      let allHotSpot = HotSpot {..} : previousHotSpots
      setLocationInCache allHotSpot
