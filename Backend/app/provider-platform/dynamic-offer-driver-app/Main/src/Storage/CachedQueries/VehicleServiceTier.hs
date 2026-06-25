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
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.VehicleServiceTier as Queries

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [VehicleServiceTier] -> m ()
createMany = Queries.createMany

findAllByMerchantOpCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Text -> m [VehicleServiceTier]
findAllByMerchantOpCityIdInRideFlow id mbSpecialLocationId = findAllByMerchantOpCityId id mbSpecialLocationId

findByServiceTierTypeAndCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => ServiceTierType -> Id DMOC.MerchantOperatingCity -> Maybe Text -> m (Maybe Domain.Types.VehicleServiceTier.VehicleServiceTier)
findByServiceTierTypeAndCityIdInRideFlow serviceTier merchantOpCityId mbSpecialLocationId = findByServiceTierTypeAndCityId serviceTier merchantOpCityId mbSpecialLocationId

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe Text -> m [VehicleServiceTier]
findAllByMerchantOpCityId merchantOpCityId mbSpecialLocationId = do
  tiers <- IM.withInMemCache [cacheKey] 3600 $ do
    Hedis.safeGet cacheKey >>= \case
      Just cachedTiers -> pure cachedTiers
      Nothing -> do
        tiers <- Queries.findAllByMerchantOpCityId merchantOpCityId
        unless (null tiers) $ do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp cacheKey tiers expTime
        pure tiers
  pure $ map (applySpecialZoneName mbSpecialLocationId) tiers
  where
    cacheKey = makeMerchantOpCityIdKey merchantOpCityId

makeMerchantOpCityIdKey :: Id DMOC.MerchantOperatingCity -> Text
makeMerchantOpCityIdKey merchantOpCityId = "CachedQueries:VehicleServiceTier:MerchantOpCityId-" <> merchantOpCityId.getId

applySpecialZoneName :: Maybe Text -> VehicleServiceTier -> VehicleServiceTier
applySpecialZoneName mbSpecialLocationId vst =
  case (mbSpecialLocationId, vst.specialZone) of
    (Just szId, Just sz) | sz.specialZoneId == szId -> vst {name = sz.serviceTierNameForZone}
    _ -> vst

findByServiceTierTypeAndCityId :: (CacheFlow m r, EsqDBFlow m r) => ServiceTierType -> Id DMOC.MerchantOperatingCity -> Maybe Text -> m (Maybe Domain.Types.VehicleServiceTier.VehicleServiceTier)
findByServiceTierTypeAndCityId serviceTier merchantOpCityId mbSpecialLocationId = do
  mbTier <- IM.withInMemCache [cacheKey] 3600 $ do
    Hedis.safeGet cacheKey >>= \case
      Just cachedTier -> pure cachedTier
      Nothing -> do
        mbTier <- Queries.findByServiceTierTypeAndCityId serviceTier merchantOpCityId
        whenJust mbTier $ \tier -> do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp cacheKey tier expTime
        pure mbTier
  pure $ fmap (applySpecialZoneName mbSpecialLocationId) mbTier
  where
    cacheKey = makeServiceTierTypeAndCityIdKey merchantOpCityId serviceTier

makeServiceTierTypeAndCityIdKey :: Id DMOC.MerchantOperatingCity -> ServiceTierType -> Text
makeServiceTierTypeAndCityIdKey merchantOpCityId serviceTier = "CachedQueries:VehicleServiceTier:MerchantOpCityId-" <> merchantOpCityId.getId <> ":ServiceTier-" <> show serviceTier

makeVehicleCategoryAndCityIdKey :: Maybe VehicleCategory -> Id DMOC.MerchantOperatingCity -> Text
makeVehicleCategoryAndCityIdKey vehicleCategory merchantOpCityId = "CachedQueries:VehicleServiceTier:MerchantOpCityId-" <> merchantOpCityId.getId <> ":vehicleCategory-" <> show vehicleCategory

findBaseServiceTierTypeByCategoryAndCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Maybe VehicleCategory -> Id DMOC.MerchantOperatingCity -> Maybe Text -> m (Maybe VehicleServiceTier)
findBaseServiceTierTypeByCategoryAndCityIdInRideFlow vehicleCategory merchantOpCityId mbSpecialLocationId = findBaseServiceTierTypeByCategoryAndCityId vehicleCategory merchantOpCityId mbSpecialLocationId

findBaseServiceTierTypeByCategoryAndCityId :: (CacheFlow m r, EsqDBFlow m r) => Maybe VehicleCategory -> Id DMOC.MerchantOperatingCity -> Maybe Text -> m (Maybe VehicleServiceTier)
findBaseServiceTierTypeByCategoryAndCityId vehicleCategory merchantOpCityId mbSpecialLocationId = do
  mbTier <- IM.withInMemCache [cacheKey] 3600 $ do
    Hedis.safeGet cacheKey >>= \case
      Just cachedTier -> pure cachedTier
      Nothing -> do
        mbTier <- Queries.findBaseServiceTierTypeByCategoryAndCityId vehicleCategory merchantOpCityId
        whenJust mbTier $ \tier -> do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp cacheKey tier expTime
        pure mbTier
  pure $ fmap (applySpecialZoneName mbSpecialLocationId) mbTier
  where
    cacheKey = makeVehicleCategoryAndCityIdKey vehicleCategory merchantOpCityId

clearCacheByServiceTier :: (CacheFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> ServiceTierType -> m ()
clearCacheByServiceTier merchantOpCityId serviceTier = do
  let key = makeServiceTierTypeAndCityIdKey merchantOpCityId serviceTier
  Hedis.runInMultiCloudRedisWrite $ Hedis.del key
  IM.refreshInMem key

clearCacheByVehicleCategory :: (CacheFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> Maybe VehicleCategory -> m ()
clearCacheByVehicleCategory merchantOpCityId vehicleCategory = do
  let key = makeVehicleCategoryAndCityIdKey vehicleCategory merchantOpCityId
  Hedis.runInMultiCloudRedisWrite $ Hedis.del key
  IM.refreshInMem key

clearCache :: (CacheFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> m ()
clearCache merchantOperatingCityId = do
  let key = makeMerchantOpCityIdKey merchantOperatingCityId
  Hedis.runInMultiCloudRedisWrite $ Hedis.del key
  IM.refreshInMem key
