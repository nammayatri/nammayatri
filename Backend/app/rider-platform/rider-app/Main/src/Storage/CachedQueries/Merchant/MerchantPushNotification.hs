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
  {-# WARNING
    "This module contains direct calls to the table and redis. \
  \ Use Storage.ConfigPilot.Config.MerchantPushNotification (getConfig) instead for reads."
    #-}
  ( create,
    findAllByMerchantOpCityId,
    findMatchingMerchantPN,
    findMatchingMerchantPNInRideFlow,
    findAllByMerchantOpCityIdInRideFlow,
    clearCache,
    updateByPrimaryKey,
  )
where

import Control.Applicative ((<|>))
import Domain.Types.MerchantOperatingCity
import Domain.Types.MerchantPushNotification
import Domain.Types.Trip
import qualified Kernel.External.Notification as Notification
import qualified Kernel.External.Types as DLanguage
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.MerchantPushNotification as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantPushNotification -> m ()
create val = do
  Queries.create val
  clearCache val.merchantOperatingCityId val.key val.tripCategory

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [MerchantPushNotification]
findAllByMerchantOpCityId id _mbConfigVersionMap =
  Hedis.safeGet (makeMerchantOpCityIdAllKey id) >>= \case
    Just configs -> return configs
    Nothing -> cacheAllMerchantPNs id /=<< Queries.findAllByMerchantOpCityId id

findMatchingMerchantPN :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> Maybe TripCategory -> Maybe Notification.SubCategory -> Maybe DLanguage.Language -> Maybe [LYT.ConfigVersionMap] -> m (Maybe MerchantPushNotification)
findMatchingMerchantPN merchantOperatingCityId messageKey tripCategory subCategory personLanguage _mbConfigVersionMap = do
  merchantPNs <- findAllByMessageKeyAndTripCategory merchantOperatingCityId messageKey tripCategory
  if null merchantPNs
    then do
      pnsWithOutTripCategory <- findAllByMessageKeyAndTripCategory merchantOperatingCityId messageKey Nothing
      return $ findMatchingNotification pnsWithOutTripCategory
    else return $ findMatchingNotification merchantPNs
  where
    findMatchingNotification pns =
      find (\pn -> Just pn.language == personLanguage && pn.fcmSubCategory == subCategory) pns
        <|> find (\pn -> pn.language == DLanguage.ENGLISH && pn.fcmSubCategory == subCategory) pns

findAllByMessageKeyAndTripCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> Maybe TripCategory -> m [MerchantPushNotification]
findAllByMessageKeyAndTripCategory merchantOperatingCityId messageKey tripCategory =
  Hedis.safeGet (makeMerchantOpCityIdAndMessageKeyAndTripCategory merchantOperatingCityId messageKey tripCategory) >>= \case
    Just configs -> return configs
    Nothing ->
      cacheMerchantPNsByKey merchantOperatingCityId messageKey tripCategory
        /=<< Queries.findAllByMerchantOpCityAndMessageKeyAndTripCategory merchantOperatingCityId messageKey tripCategory

findMatchingMerchantPNInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> Maybe TripCategory -> Maybe Notification.SubCategory -> Maybe DLanguage.Language -> [LYT.ConfigVersionMap] -> m (Maybe MerchantPushNotification)
findMatchingMerchantPNInRideFlow merchantOperatingCityId messageKey tripCategory subCategory personLanguage configVersionMap =
  findMatchingMerchantPN merchantOperatingCityId messageKey tripCategory subCategory personLanguage (Just configVersionMap)

findAllByMerchantOpCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> [LYT.ConfigVersionMap] -> m [MerchantPushNotification]
findAllByMerchantOpCityIdInRideFlow id configVersionMap =
  findAllByMerchantOpCityId id (Just configVersionMap)

cacheAllMerchantPNs :: (CacheFlow m r) => Id MerchantOperatingCity -> [MerchantPushNotification] -> m ()
cacheAllMerchantPNs cityId configs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMerchantOpCityIdAllKey cityId) configs expTime

cacheMerchantPNsByKey :: (CacheFlow m r) => Id MerchantOperatingCity -> Text -> Maybe TripCategory -> [MerchantPushNotification] -> m ()
cacheMerchantPNsByKey cityId messageKey tripCategory configs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMerchantOpCityIdAndMessageKeyAndTripCategory cityId messageKey tripCategory) configs expTime

makeMerchantOpCityIdAllKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdAllKey id = "CachedQueries:MerchantPushNotification:MerchantOperatingCityId-" <> id.getId <> "-all"

makeMerchantOpCityIdAndMessageKeyAndTripCategory :: Id MerchantOperatingCity -> Text -> Maybe TripCategory -> Text
makeMerchantOpCityIdAndMessageKeyAndTripCategory id messageKey tripCategory = "CachedQueries:MerchantPushNotification:MerchantOperatingCityId-" <> id.getId <> ":MessageKey-" <> messageKey <> ":TripCategory-" <> show tripCategory

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> Maybe TripCategory -> m ()
clearCache merchantOpCityId messageKey tripCategory = do
  Hedis.runInMultiCloudRedisWrite $ Hedis.del (makeMerchantOpCityIdAndMessageKeyAndTripCategory merchantOpCityId messageKey tripCategory)
  Hedis.runInMultiCloudRedisWrite $ Hedis.del (makeMerchantOpCityIdAllKey merchantOpCityId)

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantPushNotification -> m ()
updateByPrimaryKey cfg = do
  Queries.updateByPrimaryKey cfg
  clearCache cfg.merchantOperatingCityId cfg.key cfg.tripCategory
