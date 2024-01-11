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
  ( loadAllBaps,
    findById,
    findByShortId,
    findBySubscriberId,
    update,
    clearCache,
    getDefaultMerchantOperatingCity,
    getDefaultMerchantOperatingCity_,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Types.Registry (Subscriber)
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Merchant as Queries
import Tools.Error

loadAllBaps :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m [Merchant]
loadAllBaps = Queries.findAll

findById :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id Merchant -> m (Maybe Merchant)
findById id =
  Hedis.safeGet (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
    Nothing -> flip whenJust cacheMerchant /=<< Queries.findById id

findByShortId :: (CacheFlow m r, EsqDBFlow m r) => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId_ =
  Hedis.safeGet (makeShortIdKey shortId_) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.safeGet (makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findByShortId shortId_

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

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Merchant -> m ()
clearCache merchant = do
  Hedis.del (makeIdKey merchant.id)
  Hedis.del (makeShortIdKey merchant.shortId)

cacheMerchant :: (CacheFlow m r) => Merchant -> m ()
cacheMerchant merchant = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey merchant.id
  Hedis.setExp idKey (coerce @Merchant @(MerchantD 'Unsafe) merchant) expTime
  Hedis.setExp (makeShortIdKey merchant.shortId) idKey expTime
  Hedis.setExp (makeSubscriberIdKey merchant.subscriberId) idKey expTime

makeIdKey :: Id Merchant -> Text
makeIdKey id = "CachedQueries:Merchant:Id-" <> id.getId

makeShortIdKey :: ShortId Merchant -> Text
makeShortIdKey shortId = "CachedQueries:Merchant:ShortId-" <> shortId.getShortId

makeSubscriberIdKey :: ShortId Subscriber -> Text
makeSubscriberIdKey subscriberId = "CachedQueries:Merchant:SubscriberId-" <> subscriberId.getShortId

update :: MonadFlow m => Merchant -> m ()
update = Queries.update

-- Use only for backward compatibility
getDefaultMerchantOperatingCity :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m DMOC.MerchantOperatingCity
getDefaultMerchantOperatingCity merchantId = do
  merchant <- findById merchantId >>= fromMaybeM (MerchantDoesNotExist $ "merchantId:- " <> merchantId.getId)
  CQMOC.findByMerchantIdAndCity merchant.id merchant.defaultCity
    >>= fromMaybeM
      ( MerchantOperatingCityDoesNotExist $
          "merchantId:- " <> merchant.id.getId <> " city:- " <> show merchant.defaultCity
      )

getDefaultMerchantOperatingCity_ :: (CacheFlow m r, EsqDBFlow m r) => ShortId Merchant -> m DMOC.MerchantOperatingCity
getDefaultMerchantOperatingCity_ merchantShortId = do
  merchant <- findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist $ "merchantShortId:- " <> merchantShortId.getShortId)
  CQMOC.findByMerchantIdAndCity merchant.id merchant.defaultCity
    >>= fromMaybeM
      ( MerchantOperatingCityNotFound $
          "merchantId:- " <> merchant.id.getId <> " city:- " <> show merchant.defaultCity
      )
