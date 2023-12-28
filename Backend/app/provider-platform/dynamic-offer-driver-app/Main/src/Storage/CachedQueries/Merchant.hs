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
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant
import Kernel.Prelude
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant as Queries

findById :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id Merchant -> m (Maybe Merchant)
findById id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
    Nothing -> flip whenJust cacheMerchant /=<< Queries.findById id

findBySubscriberId :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => ShortId Subscriber -> m (Maybe Merchant)
findBySubscriberId subscriberId =
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
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeShortIdKey shortId) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findByShortId shortId

-- Call it after any update
clearCache :: HedisFlow m r => Merchant -> m ()
clearCache merchant = do
  Hedis.withCrossAppRedis $ do
    Hedis.del (makeIdKey merchant.id)
    Hedis.del (makeShortIdKey merchant.shortId)
    Hedis.del (makeSubscriberIdKey merchant.subscriberId)

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

loadAllProviders :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m [Merchant]
loadAllProviders = Queries.loadAllProviders

findAllShortIdById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Merchant] -> m [ShortId Merchant]
findAllShortIdById = Queries.findAllShortIdById
