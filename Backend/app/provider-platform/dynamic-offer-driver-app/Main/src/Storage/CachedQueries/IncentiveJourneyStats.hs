{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.IncentiveJourneyStats
  ( findByDriverIdAndJourneyIdAndPeriodKey,
    findByDriverIdAndJourneyIdAndMilestoneIdAndPeriodKey,
    upsertJourneyStats,
    clearCacheForStats,
    clearCacheByDriverJourneyPeriod,
  )
where

import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyStats as LibCQ
import Storage.Beam.IncentiveJourney ()

actor :: IJ.JourneyActor
actor = IJ.DriverActor

findByDriverIdAndJourneyIdAndPeriodKey ::
  (BeamFlow m r) =>
  Id DP.Person ->
  Id DIJ.IncentiveJourney ->
  Text ->
  m [DIJS.IncentiveJourneyStats]
findByDriverIdAndJourneyIdAndPeriodKey driverId journeyId periodKey =
  LibCQ.findByPersonIdAndJourneyIdAndPeriodKey actor (cast driverId) journeyId periodKey

findByDriverIdAndJourneyIdAndMilestoneIdAndPeriodKey ::
  (BeamFlow m r) =>
  Id DP.Person ->
  Id DIJ.IncentiveJourney ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  m (Maybe DIJS.IncentiveJourneyStats)
findByDriverIdAndJourneyIdAndMilestoneIdAndPeriodKey driverId journeyId milestoneId periodKey =
  LibCQ.findByPersonIdAndJourneyIdAndMilestoneIdAndPeriodKey actor (cast driverId) journeyId milestoneId periodKey

upsertJourneyStats ::
  (BeamFlow m r) =>
  DIJS.IncentiveJourneyStats ->
  m DIJS.IncentiveJourneyStats
upsertJourneyStats = LibCQ.upsertJourneyStats actor

clearCacheForStats :: (CacheFlow m r) => DIJS.IncentiveJourneyStats -> m ()
clearCacheForStats = LibCQ.clearCacheForStats actor

clearCacheByDriverJourneyPeriod ::
  (CacheFlow m r) =>
  Id DP.Person ->
  Id DIJ.IncentiveJourney ->
  Text ->
  m ()
clearCacheByDriverJourneyPeriod driverId journeyId periodKey =
  LibCQ.clearCacheByPersonJourneyPeriod actor (cast driverId) journeyId periodKey
