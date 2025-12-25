module Storage.Queries.FeedbackFormExtra where

import Domain.Types.FeedbackForm
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.FeedbackForm as BFF
import Storage.Queries.OrphanInstances.FeedbackForm ()

findAllFeedback :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m [FeedbackForm]
findAllFeedback = findAllWithDb [Se.Is BFF.id $ Se.Not $ Se.Eq ""]

findAllFeedbackByRating :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Int -> m [FeedbackForm]
findAllFeedbackByRating rating = findAllWithDb [Se.Or [Se.Is BFF.rating $ Se.Eq $ Just rating, Se.Is BFF.rating $ Se.Eq Nothing]]
