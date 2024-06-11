{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FeedbackForm (module Storage.Queries.FeedbackForm, module ReExport) where

import qualified Domain.Types.FeedbackForm
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FeedbackForm as Beam
import Storage.Queries.FeedbackFormExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FeedbackForm.FeedbackForm -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FeedbackForm.FeedbackForm] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FeedbackForm.FeedbackFormItem -> m (Maybe Domain.Types.FeedbackForm.FeedbackForm))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FeedbackForm.FeedbackForm -> m ())
updateByPrimaryKey (Domain.Types.FeedbackForm.FeedbackForm {..}) = do
  updateWithKV
    [ Se.Set Beam.answer answer,
      Se.Set Beam.answerType answerType,
      Se.Set Beam.categoryName categoryName,
      Se.Set Beam.question question,
      Se.Set Beam.rating rating
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

{-
	DSL Source Link: file://./../../../spec/Storage/FeedbackForm.yaml
-}
