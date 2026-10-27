{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FareProduct where

import Data.List (nub)
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import Domain.Types.FarePolicy
import Domain.Types.FareProduct
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Beam.Functions (runInMasterDb)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as Domain
import Kernel.Utils.Common (CacheFlow, MonadFlow, getCurrentTimestamp)
import qualified Lib.Types.SpecialLocation as SL
import qualified Storage.Queries.FareProduct as Queries

create :: (MonadFlow m, Esq.EsqDBFlow m r, CacheFlow m r) => FareProduct -> m ()
create = Queries.create

----------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Every FareProduct cache key of a city embeds the city's cache version, and a fare product
-- change bumps the version instead of deleting keys. Deleting could not win against a lookup
-- that read the DB just before the change and SET its result just after the delete: that
-- stale entry then lived for configsExpTime. Now such a late SET lands under the old version,
-- which nobody reads any more. Lookups must read the version BEFORE the DB, and writers bump
-- it only AFTER their DB writes. Old keys are never deleted; they expire with their TTL.
getCacheVersion :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m Integer
getCacheVersion merchantOpCityId = Hedis.withCrossAppRedis $ do
  let key = makeCacheVersionKey merchantOpCityId
  Hedis.safeGet key >>= \case
    Just version -> pure version
    Nothing -> do
      nowMs <- round <$> getCurrentTimestamp
      void $ Hedis.setNx key (nowMs :: Integer)
      fromMaybe nowMs <$> Hedis.safeGet key

bumpCacheVersion :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
bumpCacheVersion merchantOpCityId = Hedis.runInMultiCloudRedisWrite $
  Hedis.withCrossAppRedis $ do
    let key = makeCacheVersionKey merchantOpCityId
    nowMs <- round <$> getCurrentTimestamp
    void $ Hedis.setNx key (nowMs :: Integer)
    void $ Hedis.incr key

makeCacheVersionKey :: Id MerchantOperatingCity -> Text
makeCacheVersionKey merchantOpCityId = "driver-offer:CachedQueries:FareProduct:CacheVersion:MerchantOpCityId-" <> getId merchantOpCityId

----------------------------------------------------------------------------------------------------------------------------------------------------------------

findAllUnboundedFareProductForVariants :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> SL.Area -> m [FareProduct]
findAllUnboundedFareProductForVariants merchantOpCityId searchSources tripCategory area = do
  version <- getCacheVersion merchantOpCityId
  fareProducts <-
    Hedis.withCrossAppRedis (Hedis.safeGet $ makeUnboundedFareProductForVariantsByMerchantIdAndAreaKey version merchantOpCityId searchSources tripCategory area) >>= \case
      Just a -> pure a
      Nothing -> cacheAllUnboundedFareProductForVariantsByMerchantIdAndArea version merchantOpCityId searchSources tripCategory area /=<< runInMasterDb (Queries.findAllUnboundedFareProductForVariants merchantOpCityId area tripCategory Domain.Unbounded True searchSources)
  if null fareProducts && Queries.isInterCityWithCity tripCategory
    then findAllUnboundedFareProductForVariants merchantOpCityId searchSources (Queries.removeCityFromTripCategory tripCategory) area
    else pure fareProducts

cacheAllUnboundedFareProductForVariantsByMerchantIdAndArea :: (CacheFlow m r) => Integer -> Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> SL.Area -> [FareProduct] -> m ()
cacheAllUnboundedFareProductForVariantsByMerchantIdAndArea version merchantOpCityId searchSources tripCategory area fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeUnboundedFareProductForVariantsByMerchantIdAndAreaKey version merchantOpCityId searchSources tripCategory area) fareProducts expTime

makeUnboundedFareProductForVariantsByMerchantIdAndAreaKey :: Integer -> Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> SL.Area -> Text
makeUnboundedFareProductForVariantsByMerchantIdAndAreaKey version merchantOpCityId searchSources tripCategory area = "driver-offer:CachedQueries:Unbounded:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":Version-" <> show version <> ":SearchSource-" <> show searchSources <> ":Area-" <> show area <> ":TripCategory-" <> show tripCategory

----------------------------------------------------------------------------------------------------------------------------------------------------------------

findAllUnboundedFareProductForArea :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> [SearchSource] -> SL.Area -> m [FareProduct]
findAllUnboundedFareProductForArea merchantOpCityId searchSources area = do
  version <- getCacheVersion merchantOpCityId
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeUnboundedFareProductByMerchantIdAndAreaKey version merchantOpCityId searchSources area) >>= \case
    Just a -> pure a
    Nothing -> cacheAllUnboundedFareProductByMerchantIdAndArea version merchantOpCityId searchSources area /=<< runInMasterDb (Queries.findAllUnboundedFareProductForArea merchantOpCityId area Domain.Unbounded True searchSources)

cacheAllUnboundedFareProductByMerchantIdAndArea :: (CacheFlow m r) => Integer -> Id MerchantOperatingCity -> [SearchSource] -> SL.Area -> [FareProduct] -> m ()
cacheAllUnboundedFareProductByMerchantIdAndArea version merchantOpCityId searchSources area fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeUnboundedFareProductByMerchantIdAndAreaKey version merchantOpCityId searchSources area) fareProducts expTime

makeUnboundedFareProductByMerchantIdAndAreaKey :: Integer -> Id MerchantOperatingCity -> [SearchSource] -> SL.Area -> Text
makeUnboundedFareProductByMerchantIdAndAreaKey version merchantOpCityId searchSources area = "driver-offer:CachedQueries:Unbounded:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":Version-" <> show version <> ":SearchSource-" <> show searchSources <> ":Area-" <> show area

----------------------------------------------------------------------------------------------------------------------------------------------------------------

findAllFareProductByMerchantOpCityId :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> m [FareProduct]
findAllFareProductByMerchantOpCityId merchantOpCityId = do
  version <- getCacheVersion merchantOpCityId
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeFareProductByMerchantOpCityIdKey version merchantOpCityId) >>= \case
    Just a -> pure a
    Nothing -> cacheAllFareProductByMerchantOpCityId version merchantOpCityId /=<< runInMasterDb (Queries.findAllFareProductByMerchantOpCityId merchantOpCityId True)

cacheAllFareProductByMerchantOpCityId :: (CacheFlow m r) => Integer -> Id MerchantOperatingCity -> [FareProduct] -> m ()
cacheAllFareProductByMerchantOpCityId version merchantOpCityId fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeFareProductByMerchantOpCityIdKey version merchantOpCityId) fareProducts expTime

makeFareProductByMerchantOpCityIdKey :: Integer -> Id MerchantOperatingCity -> Text
makeFareProductByMerchantOpCityIdKey version merchantOpCityId = "driver-offer:CachedQueries:FareProducts:MerchantOpCityId-" <> getId merchantOpCityId <> ":Version-" <> show version

----------------------------------------------------------------------------------------------------------------------------------------------------------------

findSupportedServiceTiersByMerchantOpCityId :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> m [DVST.ServiceTierType]
findSupportedServiceTiersByMerchantOpCityId merchantOpCityId = do
  version <- getCacheVersion merchantOpCityId
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeSupportedServiceTiersKey version merchantOpCityId) >>= \case
    Just a -> pure a
    Nothing -> do
      fareProducts <- runInMasterDb $ Queries.findAllFareProductByMerchantOpCityId merchantOpCityId True
      let tiers = nub $ map (.vehicleServiceTier) fareProducts
      cacheSupportedServiceTiers version merchantOpCityId tiers
      pure tiers

cacheSupportedServiceTiers :: (CacheFlow m r) => Integer -> Id MerchantOperatingCity -> [DVST.ServiceTierType] -> m ()
cacheSupportedServiceTiers version merchantOpCityId tiers = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeSupportedServiceTiersKey version merchantOpCityId) tiers expTime

makeSupportedServiceTiersKey :: Integer -> Id MerchantOperatingCity -> Text
makeSupportedServiceTiersKey version merchantOpCityId = "driver-offer:CachedQueries:FareProducts:MerchantOpCityId-" <> getId merchantOpCityId <> ":Version-" <> show version <> ":SupportedServiceTiers"

----------------------------------------------------------------------------------------------------------------------------------------------------------------

findUnboundedByMerchantVariantArea :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> DVST.ServiceTierType -> SL.Area -> m (Maybe FareProduct)
findUnboundedByMerchantVariantArea merchantOpCityId searchSources tripCategory serviceTier area = do
  version <- getCacheVersion merchantOpCityId
  mbFareProduct <-
    Hedis.withCrossAppRedis (Hedis.safeGet $ makeUnboundedFareProductByMerchantVariantAreaKey version merchantOpCityId searchSources tripCategory serviceTier area) >>= \case
      Just a -> pure a
      Nothing -> flip whenJust (cacheUnboundedFareProductByMerchantVariantArea version merchantOpCityId searchSources tripCategory serviceTier area) /=<< runInMasterDb (Queries.findUnboundedByMerchantOpCityIdVariantArea merchantOpCityId area tripCategory serviceTier Domain.Unbounded True searchSources)
  if isNothing mbFareProduct && Queries.isInterCityWithCity tripCategory
    then findUnboundedByMerchantVariantArea merchantOpCityId searchSources (Queries.removeCityFromTripCategory tripCategory) serviceTier area
    else pure mbFareProduct

cacheUnboundedFareProductByMerchantVariantArea :: (CacheFlow m r) => Integer -> Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> DVST.ServiceTierType -> SL.Area -> FareProduct -> m ()
cacheUnboundedFareProductByMerchantVariantArea version merchantOpCityId searchSources tripCategory serviceTier area fareProduct = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeUnboundedFareProductByMerchantVariantAreaKey version merchantOpCityId searchSources tripCategory serviceTier area) fareProduct expTime

makeUnboundedFareProductByMerchantVariantAreaKey :: Integer -> Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> DVST.ServiceTierType -> SL.Area -> Text
makeUnboundedFareProductByMerchantVariantAreaKey version merchantOpCityId searchSources tripCategory serviceTier area = "driver-offer:CachedQueries:Unbounded:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":Version-" <> show version <> ":SearchSource-" <> show searchSources <> ":ServiceTier-" <> show serviceTier <> ":Area-" <> show area <> ":TripCategory-" <> show tripCategory

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

findAllBoundedByMerchantVariantArea :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> DVST.ServiceTierType -> SL.Area -> m [FareProduct]
findAllBoundedByMerchantVariantArea merchantOpCityId searchSources tripCategory serviceTier area = do
  version <- getCacheVersion merchantOpCityId
  fareProducts <-
    Hedis.withCrossAppRedis (Hedis.safeGet $ makeBoundedFareProductByMerchantVariantAreaKey version merchantOpCityId searchSources tripCategory serviceTier area) >>= \case
      Just a -> pure a
      Nothing -> cacheBoundedFareProductByMerchantVariantArea version merchantOpCityId searchSources tripCategory serviceTier area /=<< runInMasterDb (Queries.findAllBoundedByMerchantOpCityIdVariantArea merchantOpCityId area serviceTier tripCategory Domain.Unbounded True searchSources)
  -- Same city-agnostic fallback as 'findAllUnboundedFareProductForVariants'.
  if null fareProducts && Queries.isInterCityWithCity tripCategory
    then findAllBoundedByMerchantVariantArea merchantOpCityId searchSources (Queries.removeCityFromTripCategory tripCategory) serviceTier area
    else pure fareProducts

cacheBoundedFareProductByMerchantVariantArea :: (CacheFlow m r) => Integer -> Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> DVST.ServiceTierType -> SL.Area -> [FareProduct] -> m ()
cacheBoundedFareProductByMerchantVariantArea version merchantOpCityId searchSources tripCategory serviceTier area fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeBoundedFareProductByMerchantVariantAreaKey version merchantOpCityId searchSources tripCategory serviceTier area) fareProducts expTime

makeBoundedFareProductByMerchantVariantAreaKey :: Integer -> Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> DVST.ServiceTierType -> SL.Area -> Text
makeBoundedFareProductByMerchantVariantAreaKey version merchantOpCityId searchSources tripCategory serviceTier area = "driver-offer:CachedQueries:Bounded:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":Version-" <> show version <> ":SearchSource-" <> show searchSources <> ":ServiceTier-" <> show serviceTier <> ":Area-" <> show area <> ":TripCategory-" <> show tripCategory

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

findAllFareProductByMerchantOpCityIdAndArea :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> SL.Area -> m [FareProduct]
findAllFareProductByMerchantOpCityIdAndArea merchantOpCityId area = do
  version <- getCacheVersion merchantOpCityId
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeFareProductByMerchantOpCityIdAndAreaKey version merchantOpCityId area) >>= \case
    Just a -> pure a
    Nothing -> cacheFareProductByMerchantOpCityIdAndArea version merchantOpCityId area /=<< runInMasterDb (Queries.findAllFareProductByMerchantOpCityIdAndArea merchantOpCityId area True)

cacheFareProductByMerchantOpCityIdAndArea :: (CacheFlow m r) => Integer -> Id MerchantOperatingCity -> SL.Area -> [FareProduct] -> m ()
cacheFareProductByMerchantOpCityIdAndArea version merchantOpCityId area fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeFareProductByMerchantOpCityIdAndAreaKey version merchantOpCityId area) fareProducts expTime

makeFareProductByMerchantOpCityIdAndAreaKey :: Integer -> Id MerchantOperatingCity -> SL.Area -> Text
makeFareProductByMerchantOpCityIdAndAreaKey version merchantOpCityId area = "driver-offer:CachedQueries:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":Version-" <> show version <> ":Area-" <> show area

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
updateFarePolicyId :: (Esq.EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id FarePolicy -> Id FareProduct -> m ()
updateFarePolicyId = Queries.updateFarePolicyId

findAllFareProductByFarePolicyId :: (Esq.EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id FarePolicy -> m [FareProduct]
findAllFareProductByFarePolicyId farePolicyId = runInMasterDb $ Queries.findAllFareProductByFarePolicyId farePolicyId

delete :: (Esq.EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id FareProduct -> m ()
delete = Queries.delete

clearCache :: CacheFlow m r => FareProduct -> m ()
clearCache fareProduct = bumpCacheVersion fareProduct.merchantOperatingCityId

clearCacheById :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCacheById = bumpCacheVersion
