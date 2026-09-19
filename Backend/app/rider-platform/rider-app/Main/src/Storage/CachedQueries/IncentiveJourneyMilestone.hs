{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.IncentiveJourneyMilestone
  ( findByJourneyId,
    clearCacheByJourneyId,
  )
where

import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyMilestone as LibCQ
import Storage.Beam.IncentiveJourney ()

actor :: IJ.JourneyActor
actor = IJ.RiderActor

findByJourneyId ::
  (BeamFlow m r) =>
  Id DIJ.IncentiveJourney ->
  m [DIJM.IncentiveJourneyMilestone]
findByJourneyId = LibCQ.findByJourneyId actor

clearCacheByJourneyId :: (CacheFlow m r) => Id DIJ.IncentiveJourney -> m ()
clearCacheByJourneyId = LibCQ.clearCacheByJourneyId actor
