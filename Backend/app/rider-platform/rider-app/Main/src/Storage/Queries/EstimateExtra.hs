module Storage.Queries.EstimateExtra where

import Domain.Types.Estimate as DE
import Domain.Types.EstimateStatus
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Utils (mapConcurrently)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id (Id))
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as BeamE
import Storage.Queries.OrphanInstances.Estimate ()

-- Extra code goes here --

createEstimate :: (MonadFlow m, EsqDBFlow m r) => Estimate -> m ()
createEstimate estimate = do
  if fromMaybe False estimate.isMultimodalSearch then createWithKVWithOptions (Just 21600) True estimate else createWithKV estimate

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Estimate -> m ()
create = createEstimate

-- | Parallel insert. Uses 'Domain.Utils.mapConcurrently' so each Estimate is
-- written in its own forked flow; the calling thread blocks until all of
-- them complete. Errors from individual forks are swallowed by the underlying
-- helper (it returns only @Right@ values), so a write failure surfaces as a
-- missing row rather than a thrown exception — callers that need strict
-- write semantics should call 'create' sequentially.
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Forkable m) => [Estimate] -> m ()
createMany = void . mapConcurrently create

getStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Estimate -> m (Maybe EstimateStatus)
getStatus (Id estimateId) = findOneWithKV [Se.Is BeamE.id $ Se.Eq estimateId] <&> (DE.status <$>)

findBySRIdAndStatusesInKV :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Id SearchRequest -> [EstimateStatus] -> m (Maybe Estimate))
findBySRIdAndStatusesInKV requestId status = do findOneWithKVRedis [Se.And [Se.Is BeamE.requestId $ Se.Eq requestId.getId, Se.Is BeamE.status $ Se.In status]]
