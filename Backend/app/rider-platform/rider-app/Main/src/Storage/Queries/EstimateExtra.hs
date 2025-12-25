module Storage.Queries.EstimateExtra where

import Domain.Types.Estimate as DE
import Domain.Types.EstimateStatus
import Domain.Types.SearchRequest (SearchRequest)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id (Id))
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as BeamE
import qualified Storage.Queries.EstimateBreakup as QEB
import Storage.Queries.OrphanInstances.Estimate ()
import qualified Storage.Queries.TripTerms as QTT

-- Extra code goes here --

createEstimate :: (MonadFlow m, EsqDBFlow m r) => Estimate -> m ()
createEstimate estimate = do
  if fromMaybe False estimate.isMultimodalSearch then createWithKVWithOptions (Just 21600) True estimate else createWithKV estimate

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Estimate -> m ()
create estimate = do
  _ <- traverse_ QTT.create estimate.tripTerms
  _ <- createEstimate estimate
  traverse_ QEB.create estimate.estimateBreakupList

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Estimate] -> m ()
createMany = traverse_ create

getStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Estimate -> m (Maybe EstimateStatus)
getStatus (Id estimateId) = findOneWithKV [Se.Is BeamE.id $ Se.Eq estimateId] <&> (DE.status <$>)

findBySRIdAndStatusesInKV :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Id SearchRequest -> [EstimateStatus] -> m (Maybe Estimate))
findBySRIdAndStatusesInKV requestId status = do findOneWithKVRedis [Se.And [Se.Is BeamE.requestId $ Se.Eq requestId.getId, Se.Is BeamE.status $ Se.In status]]
