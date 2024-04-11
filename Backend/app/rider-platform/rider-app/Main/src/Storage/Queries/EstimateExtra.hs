{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.EstimateExtra where

import Domain.Types.Estimate as DE
import Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id (Id (Id, getId))
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as BeamE
import qualified Storage.Queries.EstimateBreakup as QEB
import Storage.Queries.OrphanInstances.Estimate
import qualified Storage.Queries.TripTerms as QTT

-- Extra code goes here --

createEstimate :: (MonadFlow m, EsqDBFlow m r) => Estimate -> m ()
createEstimate = createWithKV

create :: (MonadFlow m, EsqDBFlow m r) => Estimate -> m ()
create estimate = do
  _ <- traverse_ QTT.createTripTerms estimate.tripTerms
  _ <- createEstimate estimate
  traverse_ QEB.create estimate.estimateBreakupList

createMany :: (MonadFlow m, EsqDBFlow m r) => [Estimate] -> m ()
createMany = traverse_ create

getStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Estimate -> m (Maybe EstimateStatus)
getStatus (Id estimateId) = findOneWithKV [Se.Is BeamE.id $ Se.Eq estimateId] <&> (DE.status <$>)
