{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.IntercityPlatformFeeSlab
  ( findAllByMerchantOpCityId,
    clearCache,
  )
where

import qualified Data.List as DL
import Domain.Types.IntercityPlatformFeeSlab
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.IntercityPlatformFeeSlab as Queries

-- | One row per (city, distance bracket) -- read on every Intercity fare calculation, so cached
-- the same way FareProduct/FarePolicy already are. Same key convention as
-- Storage.CachedQueries.FareProduct.
--
-- Always returns slabs sorted ascending by minDistanceMeters -- the (city, minDistanceMeters)
-- primary key does not prevent overlapping ranges from being seeded
findAllByMerchantOpCityId :: (CacheFlow m r, Esq.EsqDBFlow m r) => Text -> m [IntercityPlatformFeeSlab]
findAllByMerchantOpCityId merchantOpCityId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeKey merchantOpCityId) >>= \case
    Just a -> pure a
    Nothing -> cacheSlabs merchantOpCityId /=<< (DL.sortOn (.minDistanceMeters) <$> Queries.findAllByMerchantOpCityId merchantOpCityId)

cacheSlabs :: (CacheFlow m r) => Text -> [IntercityPlatformFeeSlab] -> m ()
cacheSlabs merchantOpCityId slabs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeKey merchantOpCityId) slabs expTime

-- Call after any insert/update/delete against this table for a given city.
clearCache :: (CacheFlow m r) => Text -> m ()
clearCache merchantOpCityId = Hedis.withCrossAppRedis $ Hedis.del (makeKey merchantOpCityId)

makeKey :: Text -> Text
makeKey merchantOpCityId = "driver-offer:CachedQueries:IntercityPlatformFeeSlab:MerchantOpCityId-" <> merchantOpCityId
