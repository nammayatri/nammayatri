{-# OPTIONS_GHC -Wno-deprecations #-}

module Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourney
  ( findById,
    findByMerchantOperatingCityId,
    findEnabledByMerchantOperatingCityId,
    findEnabledByMerchantIdAndMerchantOperatingCityId,
    clearCache,
    clearCacheByMerchantOperatingCityId,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourney as Queries
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyExtra as QueriesExtra
import Lib.IncentiveJourney.Types.Actor (JourneyActor, actorCachePrefix)

findById ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id DIJ.IncentiveJourney ->
  m (Maybe DIJ.IncentiveJourney)
findById actor journeyId =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByIdKey actor journeyId)) >>= \case
    Just journey -> pure journey
    Nothing -> do
      mbJourney <- Queries.findById journeyId
      whenJust mbJourney $ \journey -> do
        expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
        Hedis.withCrossAppRedis $ Hedis.setExp (makeByIdKey actor journeyId) journey expTime
      pure mbJourney

findByMerchantOperatingCityId ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.MerchantOperatingCity ->
  m [DIJ.IncentiveJourney]
findByMerchantOperatingCityId actor merchantOpCityId =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByMerchantOpCityIdKey actor merchantOpCityId)) >>= \case
    Just journeys -> pure journeys
    Nothing -> cacheByMerchantOpCityId actor merchantOpCityId /=<< Queries.findByMerchantOperatingCityId Nothing Nothing merchantOpCityId

findEnabledByMerchantOperatingCityId ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.MerchantOperatingCity ->
  m [DIJ.IncentiveJourney]
findEnabledByMerchantOperatingCityId actor merchantOpCityId =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeEnabledByMerchantOpCityIdKey actor merchantOpCityId)) >>= \case
    Just journeys -> pure journeys
    Nothing -> cacheEnabledByMerchantOpCityId actor merchantOpCityId /=<< Queries.findEnabledByMerchantOperatingCityId Nothing Nothing merchantOpCityId True

findEnabledByMerchantIdAndMerchantOperatingCityId ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  m [DIJ.IncentiveJourney]
findEnabledByMerchantIdAndMerchantOperatingCityId actor merchantId merchantOpCityId =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeEnabledByMerchantIdAndMerchantOpCityIdKey actor merchantId merchantOpCityId)) >>= \case
    Just journeys -> pure journeys
    Nothing ->
      cacheEnabledByMerchantIdAndMerchantOpCityId actor merchantId merchantOpCityId
        /=<< QueriesExtra.findEnabledByMerchantIdAndMerchantOperatingCityId Nothing Nothing merchantId merchantOpCityId True

cacheByMerchantOpCityId ::
  (CacheFlow m r) =>
  JourneyActor ->
  Id Common.MerchantOperatingCity ->
  [DIJ.IncentiveJourney] ->
  m ()
cacheByMerchantOpCityId actor merchantOpCityId journeys = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeByMerchantOpCityIdKey actor merchantOpCityId) journeys expTime

cacheEnabledByMerchantOpCityId ::
  (CacheFlow m r) =>
  JourneyActor ->
  Id Common.MerchantOperatingCity ->
  [DIJ.IncentiveJourney] ->
  m ()
cacheEnabledByMerchantOpCityId actor merchantOpCityId journeys = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeEnabledByMerchantOpCityIdKey actor merchantOpCityId) journeys expTime

cacheEnabledByMerchantIdAndMerchantOpCityId ::
  (CacheFlow m r) =>
  JourneyActor ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  [DIJ.IncentiveJourney] ->
  m ()
cacheEnabledByMerchantIdAndMerchantOpCityId actor merchantId merchantOpCityId journeys = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeEnabledByMerchantIdAndMerchantOpCityIdKey actor merchantId merchantOpCityId) journeys expTime

clearCache :: (CacheFlow m r) => JourneyActor -> DIJ.IncentiveJourney -> m ()
clearCache actor journey =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $ do
      void $ Hedis.del (makeByIdKey actor journey.id)
      void $ Hedis.del (makeByMerchantOpCityIdKey actor journey.merchantOperatingCityId)
      void $ Hedis.del (makeEnabledByMerchantOpCityIdKey actor journey.merchantOperatingCityId)
      void $ Hedis.del (makeEnabledByMerchantIdAndMerchantOpCityIdKey actor journey.merchantId journey.merchantOperatingCityId)

clearCacheByMerchantOperatingCityId ::
  (CacheFlow m r) =>
  JourneyActor ->
  Id Common.MerchantOperatingCity ->
  m ()
clearCacheByMerchantOperatingCityId actor merchantOpCityId =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $ do
      void $ Hedis.del (makeByMerchantOpCityIdKey actor merchantOpCityId)
      void $ Hedis.del (makeEnabledByMerchantOpCityIdKey actor merchantOpCityId)

makeByIdKey :: JourneyActor -> Id DIJ.IncentiveJourney -> Text
makeByIdKey actor journeyId =
  actorCachePrefix actor <> ":CachedQueries:IncentiveJourney:Id-" <> journeyId.getId

makeByMerchantOpCityIdKey :: JourneyActor -> Id Common.MerchantOperatingCity -> Text
makeByMerchantOpCityIdKey actor merchantOpCityId =
  actorCachePrefix actor <> ":CachedQueries:IncentiveJourney:MerchantOperatingCityId-" <> merchantOpCityId.getId

makeEnabledByMerchantOpCityIdKey :: JourneyActor -> Id Common.MerchantOperatingCity -> Text
makeEnabledByMerchantOpCityIdKey actor merchantOpCityId =
  actorCachePrefix actor <> ":CachedQueries:IncentiveJourney:Enabled:MerchantOperatingCityId-" <> merchantOpCityId.getId

makeEnabledByMerchantIdAndMerchantOpCityIdKey :: JourneyActor -> Id Common.Merchant -> Id Common.MerchantOperatingCity -> Text
makeEnabledByMerchantIdAndMerchantOpCityIdKey actor merchantId merchantOpCityId =
  actorCachePrefix actor
    <> ":CachedQueries:IncentiveJourney:Enabled:MerchantId-"
    <> merchantId.getId
    <> ":MerchantOperatingCityId-"
    <> merchantOpCityId.getId
