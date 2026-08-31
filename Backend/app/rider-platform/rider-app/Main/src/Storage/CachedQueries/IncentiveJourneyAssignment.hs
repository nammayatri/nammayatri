{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.IncentiveJourneyAssignment
  ( findAssignmentsByUserId,
    clearCacheByPersonId,
    clearCacheByCohortMappingId,
  )
where

import qualified Domain.Types.Person as DP
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.CachedQueries.Assignment as LibCQ
import Storage.Beam.IncentiveJourney ()

actor :: IJ.JourneyActor
actor = IJ.RiderActor

findAssignmentsByUserId ::
  (BeamFlow m r) =>
  Id DP.Person ->
  m [IJ.JourneyAssignment]
findAssignmentsByUserId personId =
  LibCQ.findAssignmentsByUserId actor (cast personId)

clearCacheByPersonId :: (CacheFlow m r) => Id DP.Person -> m ()
clearCacheByPersonId personId =
  LibCQ.clearCacheByPersonId actor (cast personId)

clearCacheByCohortMappingId :: (CacheFlow m r) => Id DCJM.CohortJourneyMapping -> m ()
clearCacheByCohortMappingId = LibCQ.clearCacheByCohortMappingId actor
