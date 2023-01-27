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

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Merchant as Queries

findById :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id Merchant -> m (Maybe Merchant)
findById id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
    Nothing -> flip whenJust cacheMerchant /=<< Queries.findById id

findBySubscriberId :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => ShortId Subscriber -> m (Maybe Merchant)
findBySubscriberId subscriberId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeSubscriberIdKey subscriberId) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findBySubscriberId subscriberId

findByShortId :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => ShortId Merchant -> m (Maybe Merchant)
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
clearCache org = do
  Hedis.withCrossAppRedis $ do
    Hedis.del (makeIdKey org.id)
    Hedis.del (makeShortIdKey org.shortId)
    Hedis.del (makeSubscriberIdKey org.subscriberId)

cacheMerchant :: (HasCacheConfig r, HedisFlow m r) => Merchant -> m ()
cacheMerchant org = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey org.id
  Hedis.withCrossAppRedis $ do
    Hedis.setExp idKey (coerce @Merchant @(MerchantD 'Unsafe) org) expTime
    Hedis.setExp (makeShortIdKey org.shortId) idKey expTime
    Hedis.setExp (makeSubscriberIdKey org.subscriberId) idKey expTime

makeIdKey :: Id Merchant -> Text
makeIdKey id = "driver-offer:CachedQueries:Merchant:Id-" <> id.getId

makeSubscriberIdKey :: ShortId Subscriber -> Text
makeSubscriberIdKey subscriberId = "driver-offer:CachedQueries:Merchant:SubscriberId-" <> subscriberId.getShortId

makeShortIdKey :: ShortId Merchant -> Text
makeShortIdKey shortId = "driver-offer:CachedQueries:Merchant:ShortId-" <> shortId.getShortId

update :: Merchant -> Esq.SqlDB ()
update = Queries.update

loadAllProviders :: Esq.Transactionable m => m [Merchant]
loadAllProviders = Queries.loadAllProviders
