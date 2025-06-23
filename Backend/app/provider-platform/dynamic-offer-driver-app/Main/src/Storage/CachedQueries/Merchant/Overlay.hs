{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.Overlay where

import Domain.Types.MerchantOperatingCity
import Domain.Types.Overlay
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.Overlay as Queries
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Overlay -> m ()
create = Queries.create

findAllByMerchantOpCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> [LYT.ConfigVersionMap] -> m [Overlay]
findAllByMerchantOpCityIdInRideFlow id configVersionMap = findAllByMerchantOpCityId id (Just configVersionMap)

findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategoryInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> Language -> Maybe Text -> Maybe DVC.VehicleCategory -> [LYT.ConfigVersionMap] -> m (Maybe Overlay)
findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategoryInRideFlow id pnKey language udf1 vehicleCategory configVersionMap = findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory id pnKey language udf1 vehicleCategory (Just configVersionMap)

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [Overlay]
findAllByMerchantOpCityId id mbConfigVersionMap =
  DynamicLogic.findAllConfigs
    (cast id)
    (LYT.DRIVER_CONFIG LYT.Overlay)
    mbConfigVersionMap
    Nothing
    (Queries.findAllByMerchantOpCityId id)

findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> Language -> Maybe Text -> Maybe DVC.VehicleCategory -> Maybe [LYT.ConfigVersionMap] -> m (Maybe Overlay)
findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory id pnKey language udf1 vehicleCategory mbConfigVersionMap = do
  let cacheKey = makeMerchantIdPNKeyLangaugeUdf id pnKey language udf1 vehicleCategory
  res <-
    DynamicLogic.findOneConfigWithCacheKey
      (cast id)
      (LYT.DRIVER_CONFIG LYT.Overlay)
      mbConfigVersionMap
      Nothing
      (Queries.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory id pnKey language udf1 vehicleCategory)
      cacheKey
  case res of
    Just a -> return $ Just a
    Nothing -> case vehicleCategory of
      Nothing -> return Nothing
      _ -> findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory id pnKey language udf1 Nothing mbConfigVersionMap

makeMerchantIdPNKeyLangaugeUdf :: Id MerchantOperatingCity -> Text -> Language -> Maybe Text -> Maybe DVC.VehicleCategory -> Text
makeMerchantIdPNKeyLangaugeUdf id pnKey language udf1 mbVehicleCategory = "CachedQueries:Overlay:MerchantOpCityId-" <> id.getId <> ":PNKey-" <> show pnKey <> ":ln-" <> show language <> ":udf1-" <> show udf1 <> maybe "" ((":VehCat-" <>) . show) mbVehicleCategory

------------------------------------------------------

clearMerchantIdPNKeyLangaugeUdf :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> Language -> Maybe Text -> Maybe DVC.VehicleCategory -> m ()
clearMerchantIdPNKeyLangaugeUdf merchantOpCityId overlayKey language udf1 vehicleCategory =
  DynamicLogic.clearConfigCacheWithPrefix
    (makeMerchantIdPNKeyLangaugeUdf merchantOpCityId overlayKey language udf1 vehicleCategory)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.Overlay)
    Nothing

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m ()
clearCache merchantOperatingCityId =
  DynamicLogic.clearConfigCache
    (cast merchantOperatingCityId)
    (LYT.DRIVER_CONFIG LYT.Overlay)
    Nothing
