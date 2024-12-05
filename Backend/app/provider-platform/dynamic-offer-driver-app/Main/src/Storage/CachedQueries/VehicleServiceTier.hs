{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.VehicleServiceTier where

import Domain.Types.Common
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.VehicleCategory
import Domain.Types.VehicleServiceTier
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.VehicleServiceTier as Queries

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [VehicleServiceTier] -> m ()
createMany = Queries.createMany

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m [VehicleServiceTier]
findAllByMerchantOpCityId merchantOpCityId =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeMerchantOpCityIdKey merchantOpCityId)) >>= \case
    Just a -> return a
    Nothing -> cacheByMerchantOpCityId merchantOpCityId /=<< Queries.findAllByMerchantOpCityId merchantOpCityId

findByServiceTierTypeAndCityId :: (CacheFlow m r, EsqDBFlow m r) => ServiceTierType -> Id DMOC.MerchantOperatingCity -> m (Maybe Domain.Types.VehicleServiceTier.VehicleServiceTier)
findByServiceTierTypeAndCityId serviceTier merchantOpCityId =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeServiceTierTypeAndCityIdKey merchantOpCityId serviceTier)) >>= \case
    Just a -> return a
    Nothing -> cacheByMerchantOpCityIdAndServiceTier merchantOpCityId serviceTier /=<< Queries.findByServiceTierTypeAndCityId serviceTier merchantOpCityId

cacheByMerchantOpCityId :: (CacheFlow m r) => Id DMOC.MerchantOperatingCity -> [VehicleServiceTier] -> m ()
cacheByMerchantOpCityId merchanOperatingCityId cityServiceTiers = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMerchantOpCityIdKey merchanOperatingCityId) cityServiceTiers expTime

cacheByMerchantOpCityIdAndVehicleCategory :: (CacheFlow m r) => Maybe VehicleCategory -> Id DMOC.MerchantOperatingCity -> Maybe VehicleServiceTier -> m ()
cacheByMerchantOpCityIdAndVehicleCategory vehicleCategory merchanOperatingCityId serviceTier = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeVehicleCategoryAndCityIdKey vehicleCategory merchanOperatingCityId) serviceTier expTime

cacheByMerchantOpCityIdAndServiceTier :: (CacheFlow m r) => Id DMOC.MerchantOperatingCity -> ServiceTierType -> Maybe VehicleServiceTier -> m ()
cacheByMerchantOpCityIdAndServiceTier merchanOperatingCityId serviceTier cityServiceTier = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeServiceTierTypeAndCityIdKey merchanOperatingCityId serviceTier) cityServiceTier expTime

makeMerchantOpCityIdKey :: Id DMOC.MerchantOperatingCity -> Text
makeMerchantOpCityIdKey merchantOpCityId = "CachedQueries:VehicleServiceTier:MerchantOpCityId-" <> merchantOpCityId.getId

makeServiceTierTypeAndCityIdKey :: Id DMOC.MerchantOperatingCity -> ServiceTierType -> Text
makeServiceTierTypeAndCityIdKey merchantOpCityId serviceTier = "CachedQueries:VehicleServiceTier:MerchantOpCityId-" <> merchantOpCityId.getId <> ":ServiceTier-" <> show serviceTier

makeVehicleCategoryAndCityIdKey :: Maybe VehicleCategory -> Id DMOC.MerchantOperatingCity -> Text
makeVehicleCategoryAndCityIdKey vehicleCategory merchantOpCityId = "CachedQueries:VehicleServiceTier:MerchantOpCityId-" <> merchantOpCityId.getId <> ":vehicleCategory-" <> show vehicleCategory

findBaseServiceTierTypeByCategoryAndCityId :: (CacheFlow m r, EsqDBFlow m r) => Maybe VehicleCategory -> Id DMOC.MerchantOperatingCity -> m (Maybe VehicleServiceTier)
findBaseServiceTierTypeByCategoryAndCityId vehicleCategory merchantOpCityId = do
  Hedis.withCrossAppRedis (Hedis.safeGet (makeVehicleCategoryAndCityIdKey vehicleCategory merchantOpCityId)) >>= \case
    Just a -> return a
    Nothing -> cacheByMerchantOpCityIdAndVehicleCategory vehicleCategory merchantOpCityId /=<< Queries.findBaseServiceTierTypeByCategoryAndCityId vehicleCategory merchantOpCityId

clearCache :: Hedis.HedisFlow m r => Id DMOC.MerchantOperatingCity -> m ()
clearCache merchantOperatingCityId = do
  Hedis.del (makeMerchantOpCityIdKey merchantOperatingCityId)
