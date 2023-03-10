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
    findByExoPhone,
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

findById :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m (Maybe Merchant)
findById id =
  Hedis.safeGet (makeIdKey id) >>= \case
    Just a -> return . Just $ (coerce @(MerchantD 'Unsafe) @Merchant) a
    Nothing -> flip whenJust cacheMerchant /=<< Queries.findById id

findBySubscriberId :: (CacheFlow m r, EsqDBFlow m r) => ShortId Subscriber -> m (Maybe Merchant)
findBySubscriberId subscriberId =
  Hedis.safeGet (makeSubscriberIdKey subscriberId) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.safeGet (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findBySubscriberId subscriberId

findByShortId :: (CacheFlow m r, EsqDBFlow m r) => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId =
  Hedis.safeGet (makeShortIdKey shortId) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.safeGet (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findByShortId shortId

findByExoPhone :: (CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Merchant)
findByExoPhone exoPhone =
  Hedis.get (makeExoPhoneKey exoPhone) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.get (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findByExoPhone exoPhone

-- Call it after any update
clearCache :: HedisFlow m r => Merchant -> m ()
clearCache merchant = do
  Hedis.del (makeIdKey merchant.id)
  Hedis.del (makeShortIdKey merchant.shortId)
  Hedis.del (makeSubscriberIdKey merchant.subscriberId)
  forM_ merchant.exoPhones $ \exoPhone ->
    Hedis.del (makeExoPhoneKey exoPhone)

cacheMerchant :: (HasCacheConfig r, HedisFlow m r) => Merchant -> m ()
cacheMerchant merchant = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey merchant.id
  Hedis.setExp idKey (coerce @Merchant @(MerchantD 'Unsafe) merchant) expTime
  Hedis.setExp (makeShortIdKey merchant.shortId) idKey expTime
  Hedis.setExp (makeSubscriberIdKey merchant.subscriberId) idKey expTime
  forM_ merchant.exoPhones $ \exoPhone ->
    Hedis.setExp (makeExoPhoneKey exoPhone) idKey expTime

makeIdKey :: Id Merchant -> Text
makeIdKey id = "CachedQueries:Merchant:Id-" <> id.getId

makeSubscriberIdKey :: ShortId Subscriber -> Text
makeSubscriberIdKey subscriberId = "CachedQueries:Merchant:SubscriberId-" <> subscriberId.getShortId

makeShortIdKey :: ShortId Merchant -> Text
makeShortIdKey shortId = "CachedQueries:Merchant:ShortId-" <> shortId.getShortId

makeExoPhoneKey :: Text -> Text
makeExoPhoneKey phone = "CachedQueries:Merchant:ExoPhone-" <> phone

update :: Merchant -> Esq.SqlDB ()
update = Queries.update

loadAllProviders :: Esq.Transactionable m => m [Merchant]
loadAllProviders = Queries.loadAllProviders
