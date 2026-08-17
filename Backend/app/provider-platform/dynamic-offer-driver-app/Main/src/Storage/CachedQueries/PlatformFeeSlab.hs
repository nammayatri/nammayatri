{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.PlatformFeeSlab
  ( findAllBySlabSetId,
    findMatchingSlab,
    clearCache,
  )
where

import qualified Data.List as DL
import Domain.Types.PlatformFeeSlab
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.PlatformFeeSlab as Queries

-- | One row per (slab set, distance bracket). A fare policy opts in by pointing
-- fare_policy.platform_fee_slab_set_id at a set, so many fare policies (e.g. every Intercity
-- policy in a city) can share one set of rows. Read on every fare calculation for such a
-- policy, so cached the same way FareProduct/FarePolicy already are.
--
-- Always returns slabs sorted ascending by minDistanceMeters -- the (slabSetId, minDistanceMeters)
-- primary key does not prevent overlapping ranges from being seeded
findAllBySlabSetId :: (CacheFlow m r, Esq.EsqDBFlow m r) => Text -> m [PlatformFeeSlab]
findAllBySlabSetId slabSetId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeKey slabSetId) >>= \case
    Just a -> pure a
    Nothing -> cacheSlabs slabSetId /=<< (DL.sortOn (.minDistanceMeters) <$> Queries.findAllBySlabSetId slabSetId)

-- | Half-open range [min, max) so adjacent slabs never overlap at the boundary -- a ride at
-- exactly minDistanceMeters belongs to the slab starting there, not the one ending there.
findMatchingSlab :: (CacheFlow m r, Esq.EsqDBFlow m r) => Text -> Int -> m (Maybe PlatformFeeSlab)
findMatchingSlab slabSetId dist =
  DL.find (\s -> s.minDistanceMeters <= dist && maybe True (dist <) s.maxDistanceMeters) <$> findAllBySlabSetId slabSetId

cacheSlabs :: (CacheFlow m r) => Text -> [PlatformFeeSlab] -> m ()
cacheSlabs slabSetId slabs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeKey slabSetId) slabs expTime

-- Call after any insert/update/delete against this table for a given slab set.
clearCache :: (CacheFlow m r) => Text -> m ()
clearCache slabSetId = Hedis.withCrossAppRedis $ Hedis.del (makeKey slabSetId)

makeKey :: Text -> Text
makeKey slabSetId = "driver-offer:CachedQueries:PlatformFeeSlab:SlabSetId-" <> slabSetId
