module Storage.CachedQueries.PersonRidePatternSummary
  ( findByPersonId,
    cacheSummary,
  )
where

import Domain.Types.Person (Person)
import qualified Domain.Types.PersonRidePatternSummary as DPRPS
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.PersonRidePatternSummary as QPRPS

summaryCacheTtlSeconds :: Int
summaryCacheTtlSeconds = 3600

findByPersonId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id Person -> m (Maybe DPRPS.PersonRidePatternSummary)
findByPersonId personId =
  Hedis.safeGet (makeSummaryKey personId) >>= \case
    Just cached -> pure (Just cached)
    Nothing -> do
      mbSummary <- QPRPS.findByPersonId personId
      whenJust mbSummary cacheSummary
      pure mbSummary

cacheSummary :: (CacheFlow m r, MonadFlow m) => DPRPS.PersonRidePatternSummary -> m ()
cacheSummary summary = Hedis.setExp (makeSummaryKey summary.personId) summary summaryCacheTtlSeconds

makeSummaryKey :: Id Person -> Text
makeSummaryKey personId = "CachedQueries:PersonRidePatternSummary:PersonId-" <> personId.getId
