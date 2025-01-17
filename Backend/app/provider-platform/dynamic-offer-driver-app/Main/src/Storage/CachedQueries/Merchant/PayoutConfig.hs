{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.PayoutConfig
  ( create,
    findAllByMerchantOpCityId,
    findByPrimaryKey,
    findByMerchantOpCityIdAndIsPayoutEnabled,
    clearCache,
    clearCacheById,
    clearCacheByCategory,
    clearConfigCache,
  )
where

import Domain.Types.MerchantOperatingCity
import Domain.Types.PayoutConfig
import Domain.Types.VehicleCategory
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.PayoutConfig as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => PayoutConfig -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m [PayoutConfig]
findAllByMerchantOpCityId cityId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeCityIdKey cityId) >>= \case
    Just a -> return a
    Nothing -> cachePayoutConfigForCity' cityId /=<< Queries.findAllByMerchantOpCityId cityId

findByPrimaryKey :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> VehicleCategory -> m (Maybe PayoutConfig)
findByPrimaryKey id vehicleCategory =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKeyAndCategory id vehicleCategory) >>= \case
    Just a -> return a
    Nothing -> cachePayoutConfigForCityAndVehicleCategory id vehicleCategory /=<< Queries.findByPrimaryKey id vehicleCategory

findByMerchantOpCityIdAndIsPayoutEnabled :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Bool -> m [PayoutConfig]
findByMerchantOpCityIdAndIsPayoutEnabled id isPayoutEnabled =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id isPayoutEnabled) >>= \case
    Just a -> return a
    Nothing -> cachePayoutConfigForCity id isPayoutEnabled /=<< Queries.findByMerchantOpCityIdAndIsPayoutEnabled id isPayoutEnabled

cachePayoutConfigForCityAndVehicleCategory :: CacheFlow m r => Id MerchantOperatingCity -> VehicleCategory -> Maybe PayoutConfig -> m ()
cachePayoutConfigForCityAndVehicleCategory id vehicleCategory cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantOpCityIdKeyAndCategory id vehicleCategory
  Hedis.setExp idKey cfg expTime

cachePayoutConfigForCity :: CacheFlow m r => Id MerchantOperatingCity -> Bool -> [PayoutConfig] -> m ()
cachePayoutConfigForCity id isPayoutEnabled cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantOpCityIdKey id isPayoutEnabled
  Hedis.setExp idKey cfg expTime

cachePayoutConfigForCity' :: CacheFlow m r => Id MerchantOperatingCity -> [PayoutConfig] -> m ()
cachePayoutConfigForCity' id config = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeCityIdKey id
  Hedis.setExp idKey config expTime

makeMerchantOpCityIdKeyAndCategory :: Id MerchantOperatingCity -> VehicleCategory -> Text
makeMerchantOpCityIdKeyAndCategory id vehicleCategory = "driver-offer:CachedQueries:MerchantPayoutConfig:MerchantOperatingCityId-" <> id.getId <> ":Category-" <> show vehicleCategory

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Bool -> Text
makeMerchantOpCityIdKey id isPayoutEnabled = "driver-offer:CachedQueries:MerchantPayoutConfig:MerchantOperatingCityId-" <> id.getId <> "isPayoutEnabled:" <> show isPayoutEnabled

makeCityIdKey :: Id MerchantOperatingCity -> Text
makeCityIdKey id = "driver-offer:CachedQueries:MerchantPayoutConfig:CityId-" <> id.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> Bool -> m ()
clearCache merchantOpCityId isPayoutEnabled = Hedis.del (makeMerchantOpCityIdKey merchantOpCityId isPayoutEnabled)

clearCacheById :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCacheById merchantOpCityId = Hedis.del (makeCityIdKey merchantOpCityId)

clearCacheByCategory :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> VehicleCategory -> m ()
clearCacheByCategory merchantOpCityId vehicleCategory = Hedis.del (makeMerchantOpCityIdKeyAndCategory merchantOpCityId vehicleCategory)

clearConfigCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> VehicleCategory -> m ()
clearConfigCache merchantOperatingCityId vehicleCategory = do
  clearCacheById merchantOperatingCityId
  clearCacheByCategory merchantOperatingCityId vehicleCategory
  clearCache merchantOperatingCityId True
  clearCache merchantOperatingCityId False
