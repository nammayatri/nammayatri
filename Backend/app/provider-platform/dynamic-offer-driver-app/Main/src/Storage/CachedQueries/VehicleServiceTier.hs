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
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.VehicleServiceTier as Queries
import qualified Tools.DynamicLogic as DynamicLogic

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [VehicleServiceTier] -> m ()
createMany = Queries.createMany

findAllByMerchantOpCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> [LYT.ConfigVersionMap] -> Maybe Text -> m [VehicleServiceTier]
findAllByMerchantOpCityIdInRideFlow id configVersionMap mbSpecialLocationId = findAllByMerchantOpCityId id (Just configVersionMap) mbSpecialLocationId

findByServiceTierTypeAndCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => ServiceTierType -> Id DMOC.MerchantOperatingCity -> [LYT.ConfigVersionMap] -> Maybe Text -> m (Maybe Domain.Types.VehicleServiceTier.VehicleServiceTier)
findByServiceTierTypeAndCityIdInRideFlow serviceTier merchantOpCityId configVersionMap mbSpecialLocationId = findByServiceTierTypeAndCityId serviceTier merchantOpCityId (Just configVersionMap) mbSpecialLocationId

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> Maybe Text -> m [VehicleServiceTier]
findAllByMerchantOpCityId merchantOpCityId mbConfigVersionMap mbSpecialLocationId = do
  tiers <-
    DynamicLogic.findAllConfigs
      (cast merchantOpCityId)
      (LYT.DRIVER_CONFIG LYT.VehicleServiceTier)
      mbConfigVersionMap
      Nothing
      (Queries.findAllByMerchantOpCityId merchantOpCityId)
  pure $ map (applySpecialZoneName mbSpecialLocationId) tiers

applySpecialZoneName :: Maybe Text -> VehicleServiceTier -> VehicleServiceTier
applySpecialZoneName mbSpecialLocationId vst =
  case (mbSpecialLocationId, vst.specialZone) of
    (Just szId, Just sz) | sz.specialZoneId == szId -> vst {name = sz.serviceTierNameForZone}
    _ -> vst

findByServiceTierTypeAndCityId :: (CacheFlow m r, EsqDBFlow m r) => ServiceTierType -> Id DMOC.MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> Maybe Text -> m (Maybe Domain.Types.VehicleServiceTier.VehicleServiceTier)
findByServiceTierTypeAndCityId serviceTier merchantOpCityId mbConfigVersionMap mbSpecialLocationId = do
  mbTier <-
    DynamicLogic.findOneConfigWithCacheKey
      (cast merchantOpCityId)
      (LYT.DRIVER_CONFIG LYT.VehicleServiceTier)
      mbConfigVersionMap
      Nothing
      (Queries.findByServiceTierTypeAndCityId serviceTier merchantOpCityId)
      (makeServiceTierTypeAndCityIdKey merchantOpCityId serviceTier)
  pure $ fmap (applySpecialZoneName mbSpecialLocationId) mbTier

makeServiceTierTypeAndCityIdKey :: Id DMOC.MerchantOperatingCity -> ServiceTierType -> Text
makeServiceTierTypeAndCityIdKey merchantOpCityId serviceTier = "CachedQueries:VehicleServiceTier:MerchantOpCityId-" <> merchantOpCityId.getId <> ":ServiceTier-" <> show serviceTier

makeVehicleCategoryAndCityIdKey :: Maybe VehicleCategory -> Id DMOC.MerchantOperatingCity -> Text
makeVehicleCategoryAndCityIdKey vehicleCategory merchantOpCityId = "CachedQueries:VehicleServiceTier:MerchantOpCityId-" <> merchantOpCityId.getId <> ":vehicleCategory-" <> show vehicleCategory

findBaseServiceTierTypeByCategoryAndCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Maybe VehicleCategory -> Id DMOC.MerchantOperatingCity -> [LYT.ConfigVersionMap] -> Maybe Text -> m (Maybe VehicleServiceTier)
findBaseServiceTierTypeByCategoryAndCityIdInRideFlow vehicleCategory merchantOpCityId configsInExperimentVersions mbSpecialLocationId = findBaseServiceTierTypeByCategoryAndCityId vehicleCategory merchantOpCityId (Just configsInExperimentVersions) mbSpecialLocationId

findBaseServiceTierTypeByCategoryAndCityId :: (CacheFlow m r, EsqDBFlow m r) => Maybe VehicleCategory -> Id DMOC.MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> Maybe Text -> m (Maybe VehicleServiceTier)
findBaseServiceTierTypeByCategoryAndCityId vehicleCategory merchantOpCityId mbConfigVersionMap mbSpecialLocationId = do
  mbTier <-
    DynamicLogic.findOneConfigWithCacheKey
      (cast merchantOpCityId)
      (LYT.DRIVER_CONFIG LYT.VehicleServiceTier)
      mbConfigVersionMap
      Nothing
      (Queries.findBaseServiceTierTypeByCategoryAndCityId vehicleCategory merchantOpCityId)
      (makeVehicleCategoryAndCityIdKey vehicleCategory merchantOpCityId)
  pure $ fmap (applySpecialZoneName mbSpecialLocationId) mbTier

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m ()
clearCache merchantOperatingCityId =
  DynamicLogic.clearConfigCache
    (cast merchantOperatingCityId)
    (LYT.DRIVER_CONFIG LYT.VehicleServiceTier)
    Nothing

clearCacheByServiceTier :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> ServiceTierType -> m ()
clearCacheByServiceTier merchantOpCityId serviceTier =
  DynamicLogic.clearConfigCacheWithPrefix
    (makeServiceTierTypeAndCityIdKey merchantOpCityId serviceTier)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.VehicleServiceTier)
    Nothing

clearCacheByVehicleCategory :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe VehicleCategory -> m ()
clearCacheByVehicleCategory merchantOpCityId vehicleCategory =
  DynamicLogic.clearConfigCacheWithPrefix
    (makeVehicleCategoryAndCityIdKey vehicleCategory merchantOpCityId)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.VehicleServiceTier)
    Nothing
