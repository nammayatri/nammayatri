{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.IncentiveJourneyStats
  ( findByDriverIdAndJourneyIdAndPeriodKey,
    findByDriverIdAndJourneyIdAndMilestoneIdAndPeriodKey,
    upsertJourneyStats,
    clearCacheForStats,
    clearCacheByDriverJourneyPeriod,
  )
where

import qualified Domain.Types.IncentiveJourney as DIJ
import qualified Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Domain.Types.IncentiveJourneyStats as DIJS
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.IncentiveJourneyStats as Queries

findByDriverIdAndJourneyIdAndPeriodKey ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DIJ.IncentiveJourney ->
  Text ->
  m [DIJS.IncentiveJourneyStats]
findByDriverIdAndJourneyIdAndPeriodKey driverId journeyId periodKey =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByDriverJourneyPeriodKey driverId journeyId periodKey)) >>= \case
    Just rows -> pure rows
    Nothing -> do
      rows <- Queries.findStatsByDriverJourneyAndPeriod driverId journeyId periodKey
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.withCrossAppRedis $ Hedis.setExp (makeByDriverJourneyPeriodKey driverId journeyId periodKey) rows expTime
      pure rows

findByDriverIdAndJourneyIdAndMilestoneIdAndPeriodKey ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DIJ.IncentiveJourney ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  m (Maybe DIJS.IncentiveJourneyStats)
findByDriverIdAndJourneyIdAndMilestoneIdAndPeriodKey driverId journeyId milestoneId periodKey =
  Hedis.withCrossAppRedis (Hedis.safeGet (makeByDriverJourneyMilestonePeriodKey driverId journeyId milestoneId periodKey)) >>= \case
    Just row -> pure row
    Nothing -> do
      mbRow <- Queries.findStatsByDriverAndMilestonePeriod driverId journeyId milestoneId periodKey
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.withCrossAppRedis $ Hedis.setExp (makeByDriverJourneyMilestonePeriodKey driverId journeyId milestoneId periodKey) mbRow expTime
      pure mbRow

-- | Upsert then clear Redis keys so EndRide/list never see stale progress.
upsertJourneyStats ::
  (CacheFlow m r, EsqDBFlow m r) =>
  DIJS.IncentiveJourneyStats ->
  m DIJS.IncentiveJourneyStats
upsertJourneyStats stats = do
  updated <- Queries.upsertJourneyStats stats
  clearCacheForStats updated
  pure updated

clearCacheForStats :: (CacheFlow m r) => DIJS.IncentiveJourneyStats -> m ()
clearCacheForStats stats =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $ do
      void $ Hedis.del (makeByDriverJourneyPeriodKey stats.driverId stats.journeyId stats.periodKey)
      void $
        Hedis.del
          ( makeByDriverJourneyMilestonePeriodKey
              stats.driverId
              stats.journeyId
              stats.milestoneId
              stats.periodKey
          )

clearCacheByDriverJourneyPeriod ::
  (CacheFlow m r) =>
  Id DP.Person ->
  Id DIJ.IncentiveJourney ->
  Text ->
  m ()
clearCacheByDriverJourneyPeriod driverId journeyId periodKey =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.withCrossAppRedis $
      void $
        Hedis.del (makeByDriverJourneyPeriodKey driverId journeyId periodKey)

makeByDriverJourneyPeriodKey :: Id DP.Person -> Id DIJ.IncentiveJourney -> Text -> Text
makeByDriverJourneyPeriodKey driverId journeyId periodKey =
  "driver-offer:CachedQueries:IncentiveJourneyStats:DriverId-"
    <> driverId.getId
    <> ":JourneyId-"
    <> journeyId.getId
    <> ":PeriodKey-"
    <> periodKey

makeByDriverJourneyMilestonePeriodKey ::
  Id DP.Person ->
  Id DIJ.IncentiveJourney ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  Text
makeByDriverJourneyMilestonePeriodKey driverId journeyId milestoneId periodKey =
  "driver-offer:CachedQueries:IncentiveJourneyStats:DriverId-"
    <> driverId.getId
    <> ":JourneyId-"
    <> journeyId.getId
    <> ":MilestoneId-"
    <> milestoneId.getId
    <> ":PeriodKey-"
    <> periodKey
