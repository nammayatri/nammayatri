{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Cached access to the geohash -> area-name map.
--
-- A city holds only a few hundred cells at the precisions drivers select at, so
-- the whole city's map is fetched and cached in one go and any per-cell lookup is
-- then a pure HashMap hit. This is deliberately NOT on the dispatch path -- pool
-- matching works off the raw geohash strings, so a cold or stale cache here can
-- only ever affect the labels shown in the app, never allocation.
module Storage.CachedQueries.GeohashArea
  ( findAllByMerchantOperatingCity,
    getAreaNameMap,
    clearCacheByMerchantOperatingCity,
    mergeIntoCache,
  )
where

import qualified Data.HashMap.Strict as HashMap
import Domain.Types.GeohashArea
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.GeohashArea as Queries

findAllByMerchantOperatingCity :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> m [GeohashArea]
findAllByMerchantOperatingCity merchantOpCityId =
  Hedis.safeGet (makeGeohashAreaKeyByMerchantOperatingCityId merchantOpCityId) >>= \case
    Just a -> pure a
    Nothing -> cacheByMerchantOperatingCity merchantOpCityId /=<< Queries.findAllByMerchantOperatingCity (Just merchantOpCityId)

-- | The city's cells indexed by geohash for O(1) name resolution.
getAreaNameMap :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> m (HashMap.HashMap Text Text)
getAreaNameMap merchantOpCityId = do
  areas <- findAllByMerchantOperatingCity merchantOpCityId
  pure $ HashMap.fromList [(a.geohash, a.areaName) | a <- areas]

cacheByMerchantOperatingCity :: (CacheFlow m r) => Id MerchantOperatingCity -> [GeohashArea] -> m ()
cacheByMerchantOperatingCity merchantOpCityId areas = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeGeohashAreaKeyByMerchantOperatingCityId merchantOpCityId) areas expTime

-- | Call after any create/update of geohash_area rows for the city.
clearCacheByMerchantOperatingCity :: (CacheFlow m r) => Id MerchantOperatingCity -> m ()
clearCacheByMerchantOperatingCity = Hedis.del . makeGeohashAreaKeyByMerchantOperatingCityId

makeGeohashAreaKeyByMerchantOperatingCityId :: Id MerchantOperatingCity -> Text
makeGeohashAreaKeyByMerchantOperatingCityId merchantOpCityId = "CachedQueries:GeohashArea:MerchantOpCityId-" <> getId merchantOpCityId

-- | Reconcile the cached snapshot with rows an upsert just wrote, instead of
-- invalidating and leaving the next reader to repopulate it from Postgres.
-- The write path (createWithKV/updateWithKV) can drain to Postgres well after
-- this is called, so a read racing that window would otherwise re-cache an
-- incomplete snapshot for a full TTL -- reproduced locally: a read immediately
-- after upsert cached a list missing the new row, and kept serving it for the
-- rest of the TTL even after the row landed in Postgres. We already know
-- exactly what was written, so there is nothing to wait for.
--
-- Only touches a WARM cache. On a cold cache there is nothing to reconcile
-- against, and setting just the upserted rows would wrongly evict every other
-- area for the city, so a cold cache is left for the next reader to populate
-- fresh from Postgres.
mergeIntoCache :: (CacheFlow m r) => Id MerchantOperatingCity -> [GeohashArea] -> m ()
mergeIntoCache _ [] = pure ()
mergeIntoCache merchantOpCityId written =
  Hedis.safeGet (makeGeohashAreaKeyByMerchantOperatingCityId merchantOpCityId) >>= \case
    Nothing -> pure ()
    Just existing -> cacheByMerchantOperatingCity merchantOpCityId (reconcile existing)
  where
    writtenByGeohash = HashMap.fromList [(a.geohash, a) | a <- written]
    reconcile existing =
      [fromMaybe old (HashMap.lookup old.geohash writtenByGeohash) | old <- existing]
        <> [a | a <- written, a.geohash `notElem` map (.geohash) existing]
