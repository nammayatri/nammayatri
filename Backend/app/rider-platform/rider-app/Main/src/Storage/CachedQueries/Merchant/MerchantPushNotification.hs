{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantPushNotification
  ( create,
    findAllByMerchantOpCityId,
    findByMerchantOpCityIdAndMessageKey,
    clearCache,
  )
where

import Domain.Types.MerchantOperatingCity
import Domain.Types.MerchantPushNotification
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.MerchantPushNotification as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantPushNotification -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m [MerchantPushNotification]
findAllByMerchantOpCityId id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
    Just a -> return a
    Nothing -> cacheMerchantPushNotificationForCity id /=<< Queries.findAllByMerchantOpCityId id

cacheMerchantPushNotificationForCity :: CacheFlow m r => Id MerchantOperatingCity -> [MerchantPushNotification] -> m ()
cacheMerchantPushNotificationForCity merchantOperatingCityId cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantOpCityIdKey merchantOperatingCityId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey cfg expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey id = "CachedQueries:MerchantPushNotification:MerchantOperatingCityId-" <> id.getId

findByMerchantOpCityIdAndMessageKey :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> m (Maybe MerchantPushNotification)
findByMerchantOpCityIdAndMessageKey id messageKey =
  Hedis.safeGet (makeMerchantOpCityIdAndMessageKey id messageKey) >>= \case
    Just a -> return a
    Nothing -> flip whenJust cacheMerchantPushNotification /=<< Queries.findByMerchantOpCityIdAndMessageKey id messageKey

cacheMerchantPushNotification :: CacheFlow m r => MerchantPushNotification -> m ()
cacheMerchantPushNotification merchantPushNotification = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantOpCityIdAndMessageKey merchantPushNotification.merchantOperatingCityId merchantPushNotification.key
  Hedis.setExp idKey merchantPushNotification expTime

makeMerchantOpCityIdAndMessageKey :: Id MerchantOperatingCity -> Text -> Text
makeMerchantOpCityIdAndMessageKey id messageKey = "CachedQueries:MerchantPushNotification:MerchantOperatingCityId-" <> id.getId <> ":MessageKey-" <> messageKey

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> Text -> m ()
clearCache merchantOpCityId messageKey = do
  Hedis.del (makeMerchantOpCityIdAndMessageKey merchantOpCityId messageKey)
