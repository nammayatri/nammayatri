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

import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import Domain.Types.FarePolicy
import Domain.Types.FareProduct
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Data.List (nub)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as Domain
import Kernel.Utils.Common (CacheFlow, MonadFlow)
import qualified Lib.Types.SpecialLocation as SL
import qualified Storage.Queries.FareProduct as Queries

create :: (MonadFlow m, Esq.EsqDBFlow m r, CacheFlow m r) => FareProduct -> m ()
create = Queries.create

findAllUnboundedFareProductForVariants :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> SL.Area -> m [FareProduct]
findAllUnboundedFareProductForVariants merchantOpCityId searchSources tripCategory area =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeUnboundedFareProductForVariantsByMerchantIdAndAreaKey merchantOpCityId searchSources tripCategory area) >>= \case
    Just a -> pure a
    Nothing -> cacheAllUnboundedFareProductForVariantsByMerchantIdAndArea merchantOpCityId searchSources tripCategory area /=<< Queries.findAllUnboundedFareProductForVariants merchantOpCityId area tripCategory Domain.Unbounded True searchSources

cacheAllUnboundedFareProductForVariantsByMerchantIdAndArea :: (CacheFlow m r) => Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> SL.Area -> [FareProduct] -> m ()
cacheAllUnboundedFareProductForVariantsByMerchantIdAndArea merchantOpCityId searchSources tripCategory area fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeUnboundedFareProductForVariantsByMerchantIdAndAreaKey merchantOpCityId searchSources tripCategory area) fareProducts expTime

makeUnboundedFareProductForVariantsByMerchantIdAndAreaKey :: Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> SL.Area -> Text
makeUnboundedFareProductForVariantsByMerchantIdAndAreaKey merchantOpCityId searchSources tripCategory area = "driver-offer:CachedQueries:Unbounded:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":SearchSource-" <> show searchSources <> ":Area-" <> show area <> ":TripCategory-" <> show tripCategory

----------------------------------------------------------------------------------------------------------------------------------------------------------------

findAllUnboundedFareProductForArea :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> [SearchSource] -> SL.Area -> m [FareProduct]
findAllUnboundedFareProductForArea merchantOpCityId searchSources area =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeUnboundedFareProductByMerchantIdAndAreaKey merchantOpCityId searchSources area) >>= \case
    Just a -> pure a
    Nothing -> cacheAllUnboundedFareProductByMerchantIdAndArea merchantOpCityId searchSources area /=<< Queries.findAllUnboundedFareProductForArea merchantOpCityId area Domain.Unbounded True searchSources

cacheAllUnboundedFareProductByMerchantIdAndArea :: (CacheFlow m r) => Id MerchantOperatingCity -> [SearchSource] -> SL.Area -> [FareProduct] -> m ()
cacheAllUnboundedFareProductByMerchantIdAndArea merchantOpCityId searchSources area fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeUnboundedFareProductByMerchantIdAndAreaKey merchantOpCityId searchSources area) fareProducts expTime

makeUnboundedFareProductByMerchantIdAndAreaKey :: Id MerchantOperatingCity -> [SearchSource] -> SL.Area -> Text
makeUnboundedFareProductByMerchantIdAndAreaKey merchantOpCityId searchSources area = "driver-offer:CachedQueries:Unbounded:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":SearchSource-" <> show searchSources <> ":Area-" <> show area

----------------------------------------------------------------------------------------------------------------------------------------------------------------

findAllFareProductByMerchantOpCityId :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> m [FareProduct]
findAllFareProductByMerchantOpCityId merchantOpCityId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeFareProductByMerchantOpCityIdKey merchantOpCityId) >>= \case
    Just a -> pure a
    Nothing -> cacheAllFareProductByMerchantOpCityId merchantOpCityId /=<< Queries.findAllFareProductByMerchantOpCityId merchantOpCityId True

cacheAllFareProductByMerchantOpCityId :: (CacheFlow m r) => Id MerchantOperatingCity -> [FareProduct] -> m ()
cacheAllFareProductByMerchantOpCityId merchantOpCityId fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeFareProductByMerchantOpCityIdKey merchantOpCityId) fareProducts expTime

makeFareProductByMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeFareProductByMerchantOpCityIdKey merchantOpCityId = "driver-offer:CachedQueries:FareProducts:MerchantOpCityId-" <> getId merchantOpCityId

----------------------------------------------------------------------------------------------------------------------------------------------------------------

findSupportedServiceTiersByMerchantOpCityId :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> m [DVST.ServiceTierType]
findSupportedServiceTiersByMerchantOpCityId merchantOpCityId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeSupportedServiceTiersKey merchantOpCityId) >>= \case
    Just a -> pure a
    Nothing -> do
      fareProducts <- Queries.findAllFareProductByMerchantOpCityId merchantOpCityId True
      let tiers = nub $ map (.vehicleServiceTier) fareProducts
      cacheSupportedServiceTiers merchantOpCityId tiers
      pure tiers

cacheSupportedServiceTiers :: (CacheFlow m r) => Id MerchantOperatingCity -> [DVST.ServiceTierType] -> m ()
cacheSupportedServiceTiers merchantOpCityId tiers = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeSupportedServiceTiersKey merchantOpCityId) tiers expTime

makeSupportedServiceTiersKey :: Id MerchantOperatingCity -> Text
makeSupportedServiceTiersKey merchantOpCityId = "driver-offer:CachedQueries:FareProducts:MerchantOpCityId-" <> getId merchantOpCityId <> ":SupportedServiceTiers"

----------------------------------------------------------------------------------------------------------------------------------------------------------------

findUnboundedByMerchantVariantArea :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> DVST.ServiceTierType -> SL.Area -> m (Maybe FareProduct)
findUnboundedByMerchantVariantArea merchantOpCityId searchSources tripCategory serviceTier area =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeUnboundedFareProductByMerchantVariantAreaKey merchantOpCityId searchSources tripCategory serviceTier area) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust (cacheUnboundedFareProductByMerchantVariantArea merchantOpCityId searchSources tripCategory serviceTier area) /=<< Queries.findUnboundedByMerchantOpCityIdVariantArea merchantOpCityId area tripCategory serviceTier Domain.Unbounded True searchSources

cacheUnboundedFareProductByMerchantVariantArea :: (CacheFlow m r) => Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> DVST.ServiceTierType -> SL.Area -> FareProduct -> m ()
cacheUnboundedFareProductByMerchantVariantArea merchantOpCityId searchSources tripCategory serviceTier area fareProduct = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeUnboundedFareProductByMerchantVariantAreaKey merchantOpCityId searchSources tripCategory serviceTier area) fareProduct expTime

makeUnboundedFareProductByMerchantVariantAreaKey :: Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> DVST.ServiceTierType -> SL.Area -> Text
makeUnboundedFareProductByMerchantVariantAreaKey merchantOpCityId searchSources tripCategory serviceTier area = "driver-offer:CachedQueries:Unbounded:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":SearchSource-" <> show searchSources <> ":ServiceTier-" <> show serviceTier <> ":Area-" <> show area <> ":TripCategory-" <> show tripCategory

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

findAllBoundedByMerchantVariantArea :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> DVST.ServiceTierType -> SL.Area -> m [FareProduct]
findAllBoundedByMerchantVariantArea merchantOpCityId searchSources tripCategory serviceTier area =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeBoundedFareProductByMerchantVariantAreaKey merchantOpCityId searchSources tripCategory serviceTier area) >>= \case
    Just a -> pure a
    Nothing -> cacheBoundedFareProductByMerchantVariantArea merchantOpCityId searchSources tripCategory serviceTier area /=<< Queries.findAllBoundedByMerchantOpCityIdVariantArea merchantOpCityId area serviceTier tripCategory Domain.Unbounded True searchSources

cacheBoundedFareProductByMerchantVariantArea :: (CacheFlow m r) => Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> DVST.ServiceTierType -> SL.Area -> [FareProduct] -> m ()
cacheBoundedFareProductByMerchantVariantArea merchantOpCityId searchSources tripCategory serviceTier area fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeBoundedFareProductByMerchantVariantAreaKey merchantOpCityId searchSources tripCategory serviceTier area) fareProducts expTime

makeBoundedFareProductByMerchantVariantAreaKey :: Id MerchantOperatingCity -> [SearchSource] -> DTC.TripCategory -> DVST.ServiceTierType -> SL.Area -> Text
makeBoundedFareProductByMerchantVariantAreaKey merchantOpCityId searchSources tripCategory serviceTier area = "driver-offer:CachedQueries:Bounded:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":SearchSource-" <> show searchSources <> ":ServiceTier-" <> show serviceTier <> ":Area-" <> show area <> ":TripCategory-" <> show tripCategory

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
updateFarePolicyId :: (Esq.EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id FarePolicy -> Id FareProduct -> m ()
updateFarePolicyId = Queries.updateFarePolicyId

findAllFareProductByFarePolicyId :: (Esq.EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id FarePolicy -> m [FareProduct]
findAllFareProductByFarePolicyId = Queries.findAllFareProductByFarePolicyId

delete :: (Esq.EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id FareProduct -> m ()
delete = Queries.delete

clearCache :: CacheFlow m r => FareProduct -> m ()
clearCache FareProduct {..} = Hedis.withCrossAppRedis $ do
  let allPossibleSearchSoruces = [[ALL], [ALL, MOBILE_APP], [ALL, DASHBOARD]]
  allPossibleSearchSoruces `forM_` \searchSources -> do
    Hedis.del (makeUnboundedFareProductForVariantsByMerchantIdAndAreaKey merchantOperatingCityId searchSources tripCategory area)
    Hedis.del (makeUnboundedFareProductByMerchantIdAndAreaKey merchantOperatingCityId searchSources area)
    Hedis.del (makeFareProductByMerchantOpCityIdKey merchantOperatingCityId)
    Hedis.del (makeSupportedServiceTiersKey merchantOperatingCityId)
    Hedis.del (makeUnboundedFareProductByMerchantVariantAreaKey merchantOperatingCityId searchSources tripCategory vehicleServiceTier area)
    Hedis.del (makeBoundedFareProductByMerchantVariantAreaKey merchantOperatingCityId searchSources tripCategory vehicleServiceTier area)

clearCacheById :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCacheById merchantOperatingCityId = do
  Hedis.del (makeFareProductByMerchantOpCityIdKey merchantOperatingCityId)
  Hedis.del (makeSupportedServiceTiersKey merchantOperatingCityId)
