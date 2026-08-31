{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.IncentiveJourneyStats
  ( findByPersonIdAndJourneyIdAndPeriodKey,
    findByPersonIdAndJourneyIdAndMilestoneIdAndPeriodKey,
    upsertJourneyStats,
    clearCacheForStats,
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
actor = IJ.RiderActor

findByPersonIdAndJourneyIdAndPeriodKey ::
  (BeamFlow m r) =>
  Id DP.Person ->
  Id DIJ.IncentiveJourney ->
  Text ->
  m [DIJS.IncentiveJourneyStats]
findByPersonIdAndJourneyIdAndPeriodKey personId =
  LibCQ.findByPersonIdAndJourneyIdAndPeriodKey actor (cast personId)

findByPersonIdAndJourneyIdAndMilestoneIdAndPeriodKey ::
  (BeamFlow m r) =>
  Id DP.Person ->
  Id DIJ.IncentiveJourney ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  m (Maybe DIJS.IncentiveJourneyStats)
findByPersonIdAndJourneyIdAndMilestoneIdAndPeriodKey personId =
  LibCQ.findByPersonIdAndJourneyIdAndMilestoneIdAndPeriodKey actor (cast personId)

upsertJourneyStats ::
  (BeamFlow m r) =>
  DIJS.IncentiveJourneyStats ->
  m DIJS.IncentiveJourneyStats
upsertJourneyStats = LibCQ.upsertJourneyStats actor

clearCacheForStats :: (CacheFlow m r) => DIJS.IncentiveJourneyStats -> m ()
clearCacheForStats = LibCQ.clearCacheForStats actor
