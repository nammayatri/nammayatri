{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant
  ( findById,
    findByShortId,
    findBySubscriberId,
    update,
    loadAllProviders,
    clearCache,
  )
where

import Domain.Types.Common
import Domain.Types.Merchant
import GHC.Base (coerce)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Merchant as Queries

findById :: forall m r. (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id Merchant -> m (Maybe Merchant)
findById id =
  Hedis.safeGet (makeIdKey id) >>= \case
    Just a -> return . Just $ (coerce @(MerchantD 'Unsafe) @Merchant) a
    Nothing -> flip whenJust cacheMerchant /=<< Queries.findById id (Proxy @m)

findBySubscriberId :: forall m r. (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => ShortId Subscriber -> m (Maybe Merchant)
findBySubscriberId subscriberId =
  Hedis.safeGet (makeSubscriberIdKey subscriberId) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.safeGet (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findBySubscriberId subscriberId (Proxy @m)

findByShortId :: forall m r. (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId =
  Hedis.safeGet (makeShortIdKey shortId) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.safeGet (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findByShortId shortId (Proxy @m)

-- Call it after any update
clearCache :: HedisFlow m r => Merchant -> m ()
clearCache org = do
  Hedis.del (makeIdKey org.id)
  Hedis.del (makeShortIdKey org.shortId)
  Hedis.del (makeSubscriberIdKey org.subscriberId)

cacheMerchant :: (HasCacheConfig r, HedisFlow m r) => Merchant -> m ()
cacheMerchant org = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey org.id
  Hedis.setExp idKey (coerce @Merchant @(MerchantD 'Unsafe) org) expTime
  Hedis.setExp (makeShortIdKey org.shortId) idKey expTime
  Hedis.setExp (makeSubscriberIdKey org.subscriberId) idKey expTime

makeIdKey :: Id Merchant -> Text
makeIdKey id = "CachedQueries:Merchant:Id-" <> id.getId

makeSubscriberIdKey :: ShortId Subscriber -> Text
makeSubscriberIdKey subscriberId = "CachedQueries:Merchant:SubscriberId-" <> subscriberId.getShortId

makeShortIdKey :: ShortId Merchant -> Text
makeShortIdKey shortId = "CachedQueries:Merchant:ShortId-" <> shortId.getShortId

update :: HedisFlow m r => Merchant -> Esq.SqlDB m ()
update merchant = do
  Queries.update merchant
  Esq.finalize $ clearCache merchant

loadAllProviders :: forall m ma. Esq.Transactionable ma m => Proxy ma -> m [Merchant]
loadAllProviders = Queries.loadAllProviders
