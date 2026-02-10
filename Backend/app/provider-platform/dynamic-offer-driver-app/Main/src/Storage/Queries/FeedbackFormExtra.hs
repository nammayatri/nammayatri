module Storage.Queries.FeedbackFormExtra where

import Domain.Types.FeedbackForm
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.FeedbackForm as BFF
import Storage.Queries.OrphanInstances.FeedbackForm ()

deleteById :: (MonadFlow m, EsqDBFlow m r) => Kernel.Types.Id.Id FeedbackFormItem -> m ()
deleteById feedbackFormId = deleteWithKV [Se.Is BFF.id $ Se.Eq (Kernel.Types.Id.getId feedbackFormId)]

findAllFeedback :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m [FeedbackForm]
findAllFeedback = findAllWithDb [Se.Is BFF.id $ Se.Not $ Se.Eq ""]

findAllFeedbackByRating :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Int -> m [FeedbackForm]
findAllFeedbackByRating rating = findAllWithDb [Se.Or [Se.Is BFF.rating $ Se.Eq $ Just rating, Se.Is BFF.rating $ Se.Eq Nothing]]
