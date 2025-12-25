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
    findMatchingMerchantPNInRideFlow,
    findAllByMerchantOpCityIdInRideFlow,
    clearCache,
  )
where

import Control.Applicative ((<|>))
import Domain.Types.MerchantOperatingCity
import Domain.Types.MerchantPushNotification
import Domain.Types.Trip
import qualified Kernel.External.Notification as Notification
import qualified Kernel.External.Types as DLanguage
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.MerchantPushNotification as Queries
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantPushNotification -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [MerchantPushNotification]
findAllByMerchantOpCityId id mbConfigVersionMap =
  DynamicLogic.findAllConfigs (cast id) (LYT.RIDER_CONFIG LYT.MerchantPushNotification) mbConfigVersionMap Nothing (Queries.findAllByMerchantOpCityId id)

findMatchingMerchantPN :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> Maybe TripCategory -> Maybe Notification.SubCategory -> Maybe DLanguage.Language -> Maybe [LYT.ConfigVersionMap] -> m (Maybe MerchantPushNotification)
findMatchingMerchantPN merchantOperatingCityId messageKey tripCategory subCategory personLanguage mbConfigVersionMap = do
  merchantPNs <-
    DynamicLogic.findAllConfigsWithCacheKey
      (cast merchantOperatingCityId)
      (LYT.RIDER_CONFIG LYT.MerchantPushNotification)
      mbConfigVersionMap
      Nothing
      (Queries.findAllByMerchantOpCityAndMessageKeyAndTripCategory merchantOperatingCityId messageKey tripCategory)
      (makeMerchantOpCityIdAndMessageKeyAndTripCategory merchantOperatingCityId messageKey tripCategory)

  if null merchantPNs
    then do
      pnsWithOutTripCategory <-
        DynamicLogic.findAllConfigsWithCacheKey
          (cast merchantOperatingCityId)
          (LYT.RIDER_CONFIG LYT.MerchantPushNotification)
          mbConfigVersionMap
          Nothing
          (Queries.findAllByMerchantOpCityAndMessageKeyAndTripCategory merchantOperatingCityId messageKey Nothing)
          (makeMerchantOpCityIdAndMessageKeyAndTripCategory merchantOperatingCityId messageKey Nothing)
      return $ findMatchingNotification pnsWithOutTripCategory
    else return $ findMatchingNotification merchantPNs
  where
    findMatchingNotification pns =
      find (\pn -> Just pn.language == personLanguage && pn.fcmSubCategory == subCategory) pns
        <|> find (\pn -> pn.language == DLanguage.ENGLISH && pn.fcmSubCategory == subCategory) pns

findMatchingMerchantPNInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> Maybe TripCategory -> Maybe Notification.SubCategory -> Maybe DLanguage.Language -> [LYT.ConfigVersionMap] -> m (Maybe MerchantPushNotification)
findMatchingMerchantPNInRideFlow merchantOperatingCityId messageKey tripCategory subCategory personLanguage configVersionMap =
  findMatchingMerchantPN merchantOperatingCityId messageKey tripCategory subCategory personLanguage (Just configVersionMap)

findAllByMerchantOpCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> [LYT.ConfigVersionMap] -> m [MerchantPushNotification]
findAllByMerchantOpCityIdInRideFlow id configVersionMap =
  findAllByMerchantOpCityId id (Just configVersionMap)

makeMerchantOpCityIdAndMessageKeyAndTripCategory :: Id MerchantOperatingCity -> Text -> Maybe TripCategory -> Text
makeMerchantOpCityIdAndMessageKeyAndTripCategory id messageKey tripCategory = "CachedQueries:MerchantPushNotification:MerchantOperatingCityId-" <> id.getId <> ":MessageKey-" <> messageKey <> ":TripCategory-" <> show tripCategory

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> Maybe TripCategory -> m ()
clearCache merchantOpCityId messageKey tripCategory =
  DynamicLogic.clearConfigCacheWithPrefix
    (makeMerchantOpCityIdAndMessageKeyAndTripCategory merchantOpCityId messageKey tripCategory)
    (cast merchantOpCityId)
    (LYT.RIDER_CONFIG LYT.MerchantPushNotification)
    Nothing
