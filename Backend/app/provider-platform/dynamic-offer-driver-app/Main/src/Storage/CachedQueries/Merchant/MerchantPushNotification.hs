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
    findMatchingMerchantPN,
    clearCache,
    clearCacheById,
  )
where

import Control.Applicative ((<|>))
import Domain.Types.MerchantOperatingCity
import Domain.Types.MerchantPushNotification
import Domain.Types.Trip
import qualified Kernel.External.Notification as Notification
import Kernel.External.Types
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
makeMerchantOpCityIdKey id = "driver-offer:CachedQueries:MerchantPushNotification:MerchantOperatingCityId-" <> id.getId

findMatchingMerchantPN :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> Maybe TripCategory -> Maybe Notification.SubCategory -> Maybe Language -> m (Maybe MerchantPushNotification)
findMatchingMerchantPN merchantOperatingCityId messageKey tripCategory subCategory personLanguage = do
  merchantPNs <-
    Hedis.safeGet (makeMerchantOpCityIdAndMessageKeyAndTripCategory merchantOperatingCityId messageKey tripCategory) >>= \case
      Just a -> return a
      Nothing -> do
        pns <- Queries.findAllByMerchantOpCityIdAndMessageKeyAndTripCategory merchantOperatingCityId messageKey tripCategory
        if null pns
          then do
            Hedis.safeGet (makeMerchantOpCityIdAndMessageKeyAndTripCategory merchantOperatingCityId messageKey Nothing) >>= \case
              Just a' -> return a'
              Nothing -> do
                pnsWithOutTripCategory <- Queries.findAllByMerchantOpCityIdAndMessageKeyAndTripCategory merchantOperatingCityId messageKey Nothing
                cacheMerchantPushNotification merchantOperatingCityId messageKey Nothing pnsWithOutTripCategory
                return pnsWithOutTripCategory
          else do
            cacheMerchantPushNotification merchantOperatingCityId messageKey tripCategory pns
            return pns
  let matchingPN =
        find (\pn -> Just pn.language == personLanguage && pn.fcmSubCategory == subCategory) merchantPNs
          <|> find (\pn -> pn.language == ENGLISH && pn.fcmSubCategory == subCategory) merchantPNs
  return matchingPN

cacheMerchantPushNotification :: CacheFlow m r => Id MerchantOperatingCity -> Text -> Maybe TripCategory -> [MerchantPushNotification] -> m ()
cacheMerchantPushNotification id messageKey tripCategory merchantPushNotification = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantOpCityIdAndMessageKeyAndTripCategory id messageKey tripCategory
  Hedis.setExp idKey merchantPushNotification expTime

makeMerchantOpCityIdAndMessageKeyAndTripCategory :: Id MerchantOperatingCity -> Text -> Maybe TripCategory -> Text
makeMerchantOpCityIdAndMessageKeyAndTripCategory id messageKey tripCategory = "CachedQueries:MerchantPushNotification:MerchantOperatingCityId-" <> id.getId <> ":MessageKey-" <> messageKey <> ":TripCategory-" <> show tripCategory

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> Text -> Maybe TripCategory -> m ()
clearCache merchantOpCityId messageKey tripCategory = do
  Hedis.del (makeMerchantOpCityIdAndMessageKeyAndTripCategory merchantOpCityId messageKey tripCategory)

clearCacheById :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCacheById merchantOpCityId = do
  Hedis.del (makeMerchantOpCityIdKey merchantOpCityId)
