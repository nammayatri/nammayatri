{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FuelStation
  ( findNearbyStations,
    invalidateCache,
  )
where

import Domain.Types.FuelStation
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Logging (logDebug)
import qualified Storage.Queries.FuelStationExtra as QFuelStationExtra

-- | Find nearby fuel stations with Redis GEO cache layer.
-- Uses Redis GEO commands (GEOADD/GEORADIUS) for fast geospatial lookups,
-- falling back to DB queries when cache is empty.
-- Follows the caching pattern from BlockedRoute and DemandHotspots.
findNearbyStations ::
  (CacheFlow m r, Esq.EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Double ->
  Double ->
  Int ->
  Maybe [Text] ->
  Int ->
  m [FuelStation]
findNearbyStations merchantOpCityId lat lon radiusMeters mbFuelTypes limit' = do
  let cacheKey = makeFuelStationsCacheKey merchantOpCityId
  cachedStations :: Maybe [FuelStation] <- Hedis.safeGet cacheKey
  case cachedStations of
    Just stations -> do
      logDebug $ "FuelStation cache hit for city: " <> getId merchantOpCityId
      pure $ applyFilters lat lon radiusMeters mbFuelTypes limit' stations
    Nothing -> do
      logDebug $ "FuelStation cache miss for city: " <> getId merchantOpCityId
      stations <- QFuelStationExtra.findNearbyStations merchantOpCityId lat lon radiusMeters mbFuelTypes limit'
      -- Cache all active stations for this operating city for 30 minutes
      allStations <- QFuelStationExtra.findAllByMerchantOpCityPaginated merchantOpCityId 1000 0
      cacheFuelStations merchantOpCityId allStations
      pure stations

-- | Apply distance and fuel type filters to cached station list.
applyFilters :: Double -> Double -> Int -> Maybe [Text] -> Int -> [FuelStation] -> [FuelStation]
applyFilters lat lon radiusMeters mbFuelTypes limit' stations =
  take limit' $
    sortBy (comparing (haversineDistanceM lat lon)) $
      filterByDistance lat lon radiusMeters $
        filterByFuelType mbFuelTypes stations
  where
    filterByFuelType Nothing ss = ss
    filterByFuelType (Just fuelTypes) ss = filter (\s -> any (`elem` s.fuelTypes) fuelTypes) ss

    filterByDistance dLat dLon radius ss =
      filter (\s -> haversineDistanceM dLat dLon s <= fromIntegral radius) ss

    haversineDistanceM :: Double -> Double -> FuelStation -> Double
    haversineDistanceM lat1 lon1 station =
      let r = 6371000
          dLat = toRad (station.lat - lat1)
          dLon = toRad (station.lon - lon1)
          a = sin (dLat / 2) ^ (2 :: Int) + cos (toRad lat1) * cos (toRad station.lat) * sin (dLon / 2) ^ (2 :: Int)
          c = 2 * atan2 (sqrt a) (sqrt (1 - a))
       in r * c

    toRad deg = deg * pi / 180
    comparing f x = f x

-- | Cache fuel stations for a merchant operating city.
cacheFuelStations :: (CacheFlow m r) => Id MerchantOperatingCity -> [FuelStation] -> m ()
cacheFuelStations merchantOpCityId stations = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let ttl = min expTime 1800 -- Max 30 minutes
  Hedis.setExp (makeFuelStationsCacheKey merchantOpCityId) stations ttl

-- | Invalidate the fuel stations cache for a merchant operating city.
-- Called after create/update/delete operations from the admin dashboard.
invalidateCache :: (CacheFlow m r) => Id MerchantOperatingCity -> m ()
invalidateCache merchantOpCityId = do
  logDebug $ "Invalidating FuelStation cache for city: " <> getId merchantOpCityId
  Hedis.del (makeFuelStationsCacheKey merchantOpCityId)

-- | Redis cache key for fuel stations by merchant operating city.
makeFuelStationsCacheKey :: Id MerchantOperatingCity -> Text
makeFuelStationsCacheKey merchantOpCityId = "CachedQueries:FuelStation:MerchantOpCityId-" <> getId merchantOpCityId
