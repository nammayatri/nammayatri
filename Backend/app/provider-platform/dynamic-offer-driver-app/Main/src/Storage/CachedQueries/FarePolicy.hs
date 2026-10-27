{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FarePolicy
  {-# WARNING
    "This module contains direct calls to the table and redis. \
  \ But most likely you need a version from Cac with inMem results feature."
    #-}
  ( clearCache,
    create,
    delete,
    update',
    clearCacheById,
    findFarePolicyFromDB,
  )
where

import Data.Coerce (coerce)
import Data.Text as Text
import Domain.Types.Common
import Domain.Types.FarePolicy
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.FarePolicy as Queries

-- Short TTL: clearCache also drops this entry on every pod, the TTL only bounds staleness if that propagation is missed.
inMemCacheTtl :: Seconds
inMemCacheTtl = 30

findFarePolicyFromDB :: (CacheFlow m r, EsqDBFlow m r) => Id FarePolicy -> m (Maybe FarePolicy)
findFarePolicyFromDB id =
  IM.withInMemCache [makeIdKey id] inMemCacheTtl $
    Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
      Just a -> return . Just $ coerce @(FarePolicyD 'Unsafe) @FarePolicy a
      Nothing -> do
        flip whenJust cacheFarePolicy /=<< Queries.findById id

cacheFarePolicy :: (CacheFlow m r) => FarePolicy -> m ()
cacheFarePolicy fp = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey fp.id
  Hedis.withCrossAppRedis $ do
    Hedis.setExp idKey (coerce @FarePolicy @(FarePolicyD 'Unsafe) fp) expTime

makeIdKey :: Id FarePolicy -> Text
makeIdKey id = "driver-offer:CachedQueries:FarePolicy:Id-" <> id.getId

-- Call it after any update
clearCache :: (CacheFlow m r, MonadFlow m) => FarePolicy -> m ()
clearCache fp = clearCacheById fp.id

clearCacheById :: (CacheFlow m r, MonadFlow m) => Id FarePolicy -> m ()
clearCacheById fid = do
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $ do
      Hedis.del (makeIdKey fid)
  -- Also drop the in-mem entries on all pods, so an update isn't masked by a stale in-process copy.
  IM.refreshInMem (makeIdKey fid)

-- Clears the cache too: a policy can be deleted and re-created under the same id, and a lookup in between would have cached Nothing in memory.
create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => FarePolicy -> m ()
create fp = do
  Queries.create fp
  clearCacheById fp.id

delete :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id FarePolicy -> m ()
delete farePolicyId = do
  Queries.delete farePolicyId
  clearCacheById farePolicyId

update' :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => FarePolicy -> m ()
update' farePolicy = do
  Queries.update' farePolicy
  clearCacheById farePolicy.id
