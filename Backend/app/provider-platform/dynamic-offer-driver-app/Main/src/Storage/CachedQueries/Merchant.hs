{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant
  ( findById,
    findByShortId,
    findBySubscriberId,
    update,
    loadAllProviders,
    clearCache,
    findAllShortIdById,
    updateGeofencingConfig,
    updateGatewayAndRegistryPriorityList,
  )
where

import Data.Coerce (coerce)
import qualified Domain.Types
import Domain.Types.Common
import Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant as Queries

-- | In-memory (L1) cache TTL for merchant lookups. Merchant rows are config-like
--   and rarely change; a short-lived in-process cache avoids the Redis round-trip
--   on the hot path. NOTE: 'clearCache' only clears Redis, so an updated merchant
--   may be served stale from a pod's in-mem cache for up to this many seconds.
inMemCacheTtl :: Seconds
inMemCacheTtl = 3600

findById :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id Merchant -> m (Maybe Merchant)
findById id =
  IM.withInMemCache [makeIdKey id] inMemCacheTtl $
    Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
      Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
      Nothing -> flip whenJust cacheMerchant /=<< Queries.findById id

findBySubscriberId :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => ShortId Subscriber -> m (Maybe Merchant)
findBySubscriberId subscriberId =
  IM.withInMemCache [makeSubscriberIdKey subscriberId] inMemCacheTtl $
    Hedis.withCrossAppRedis (Hedis.safeGet $ makeSubscriberIdKey subscriberId) >>= \case
      Nothing -> findAndCache
      Just id ->
        Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
          Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
          Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findBySubscriberId subscriberId

findByShortId :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId =
  IM.withInMemCache [makeShortIdKey shortId] inMemCacheTtl $
    Hedis.withCrossAppRedis (Hedis.safeGet $ makeShortIdKey shortId) >>= \case
      Nothing -> findAndCache
      Just id ->
        Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
          Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
          Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findByShortId shortId

-- Call it after any update
clearCache :: (CacheFlow m r, MonadFlow m) => Merchant -> m ()
clearCache merchant = do
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $ do
      Hedis.del (makeIdKey merchant.id)
      Hedis.del (makeShortIdKey merchant.shortId)
      Hedis.del (makeSubscriberIdKey merchant.subscriberId)
  -- Also drop the L1 in-mem entries (and propagate the cleanup to other pods via
  -- the InMem sidecar) so an update isn't masked by a stale in-process cache.
  IM.refreshInMem (makeIdKey merchant.id)
  IM.refreshInMem (makeShortIdKey merchant.shortId)
  IM.refreshInMem (makeSubscriberIdKey merchant.subscriberId)

cacheMerchant :: CacheFlow m r => Merchant -> m ()
cacheMerchant merchant = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey merchant.id
  Hedis.withCrossAppRedis $ do
    Hedis.setExp idKey (coerce @Merchant @(MerchantD 'Unsafe) merchant) expTime
    Hedis.setExp (makeShortIdKey merchant.shortId) idKey expTime
    Hedis.setExp (makeSubscriberIdKey merchant.subscriberId) idKey expTime

makeIdKey :: Id Merchant -> Text
makeIdKey id = "driver-offer:CachedQueries:Merchant:Id-" <> id.getId

makeSubscriberIdKey :: ShortId Subscriber -> Text
makeSubscriberIdKey subscriberId = "driver-offer:CachedQueries:Merchant:SubscriberId-" <> subscriberId.getShortId

makeShortIdKey :: ShortId Merchant -> Text
makeShortIdKey shortId = "driver-offer:CachedQueries:Merchant:ShortId-" <> shortId.getShortId

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Merchant -> m ()
update = Queries.update

updateGeofencingConfig :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> GeoRestriction -> GeoRestriction -> m ()
updateGeofencingConfig = Queries.updateGeofencingConfig

loadAllProviders :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m [Merchant]
loadAllProviders = Queries.loadAllProviders

findAllShortIdById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Merchant] -> m [ShortId Merchant]
findAllShortIdById = Queries.findAllShortIdById

updateGatewayAndRegistryPriorityList :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Merchant -> [Domain.Types.GatewayAndRegistryService] -> m ()
updateGatewayAndRegistryPriorityList merchant gatewayAndRegistryPriorityList = do
  Queries.updateGatewayAndRegistryPriorityList gatewayAndRegistryPriorityList merchant.id
  clearCache merchant
