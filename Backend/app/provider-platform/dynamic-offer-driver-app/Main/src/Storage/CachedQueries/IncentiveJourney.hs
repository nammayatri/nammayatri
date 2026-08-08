{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.IncentiveJourney
  ( findById,
    findByMerchantOperatingCityId,
    findEnabledByMerchantOperatingCityId,
    clearCache,
    clearCacheByMerchantOperatingCityId,
  )
where

import qualified Domain.Types.IncentiveJourney as DIJ
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.IncentiveJourney as Queries

findById ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DIJ.IncentiveJourney ->
  m (Maybe DIJ.IncentiveJourney)
findById journeyId =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByIdKey journeyId)) >>= \case
    Just journey -> pure journey
    Nothing -> do
      mbJourney <- Queries.findById journeyId
      whenJust mbJourney $ \journey -> do
        expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
        Hedis.withCrossAppRedis $ Hedis.setExp (makeByIdKey journeyId) journey expTime
      pure mbJourney

findByMerchantOperatingCityId ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [DIJ.IncentiveJourney]
findByMerchantOperatingCityId merchantOpCityId =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByMerchantOpCityIdKey merchantOpCityId)) >>= \case
    Just journeys -> pure journeys
    Nothing -> cacheByMerchantOpCityId merchantOpCityId /=<< Queries.findByMerchantOperatingCityId Nothing Nothing merchantOpCityId

findEnabledByMerchantOperatingCityId ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [DIJ.IncentiveJourney]
findEnabledByMerchantOperatingCityId merchantOpCityId =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeEnabledByMerchantOpCityIdKey merchantOpCityId)) >>= \case
    Just journeys -> pure journeys
    Nothing -> cacheEnabledByMerchantOpCityId merchantOpCityId /=<< Queries.findEnabledByMerchantOperatingCityId Nothing Nothing merchantOpCityId True

cacheByMerchantOpCityId :: (MonadFlow m, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> [DIJ.IncentiveJourney] -> m ()
cacheByMerchantOpCityId merchantOpCityId journeys = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeByMerchantOpCityIdKey merchantOpCityId) journeys expTime

cacheEnabledByMerchantOpCityId :: (MonadFlow m, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> [DIJ.IncentiveJourney] -> m ()
cacheEnabledByMerchantOpCityId merchantOpCityId journeys = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeEnabledByMerchantOpCityIdKey merchantOpCityId) journeys expTime

-- | Clear journey-id key and city list keys after create/update on both clouds.
clearCache :: (CacheFlow m r) => DIJ.IncentiveJourney -> m ()
clearCache journey =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $ do
      void $ Hedis.del (makeByIdKey journey.id)
      void $ Hedis.del (makeByMerchantOpCityIdKey journey.merchantOperatingCityId)
      void $ Hedis.del (makeEnabledByMerchantOpCityIdKey journey.merchantOperatingCityId)

clearCacheByMerchantOperatingCityId :: (CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m ()
clearCacheByMerchantOperatingCityId merchantOpCityId =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $ do
      void $ Hedis.del (makeByMerchantOpCityIdKey merchantOpCityId)
      void $ Hedis.del (makeEnabledByMerchantOpCityIdKey merchantOpCityId)

makeByIdKey :: Id DIJ.IncentiveJourney -> Text
makeByIdKey journeyId = "driver-offer:CachedQueries:IncentiveJourney:Id-" <> journeyId.getId

makeByMerchantOpCityIdKey :: Id DMOC.MerchantOperatingCity -> Text
makeByMerchantOpCityIdKey merchantOpCityId =
  "driver-offer:CachedQueries:IncentiveJourney:MerchantOperatingCityId-" <> merchantOpCityId.getId

makeEnabledByMerchantOpCityIdKey :: Id DMOC.MerchantOperatingCity -> Text
makeEnabledByMerchantOpCityIdKey merchantOpCityId =
  "driver-offer:CachedQueries:IncentiveJourney:Enabled:MerchantOperatingCityId-" <> merchantOpCityId.getId
