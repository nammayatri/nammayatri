{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FeedbackForm where

import qualified Domain.Types.FeedbackForm
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FeedbackForm as Beam

instance FromTType' Beam.FeedbackForm Domain.Types.FeedbackForm.FeedbackForm where
  fromTType' (Beam.FeedbackFormT {..}) = do
    pure $
      Just
        Domain.Types.FeedbackForm.FeedbackForm
          { categoryName = categoryName,
            id = Kernel.Types.Id.Id id,
            rating = rating,
            question = question,
            answer = answer,
            answerType = answerType
          }

instance ToTType' Beam.FeedbackForm Domain.Types.FeedbackForm.FeedbackForm where
  toTType' (Domain.Types.FeedbackForm.FeedbackForm {..}) = do
    Beam.FeedbackFormT
      { Beam.categoryName = categoryName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.rating = rating,
        Beam.question = question,
        Beam.answer = answer,
        Beam.answerType = answerType
      }
