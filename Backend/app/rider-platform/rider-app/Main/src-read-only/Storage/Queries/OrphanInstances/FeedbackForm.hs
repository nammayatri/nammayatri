{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FeedbackForm where

import qualified Domain.Types.FeedbackForm
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FeedbackForm as Beam

instance FromTType' Beam.FeedbackForm Domain.Types.FeedbackForm.FeedbackForm where
  fromTType' (Beam.FeedbackFormT {..}) = do
    pure $
      Just
        Domain.Types.FeedbackForm.FeedbackForm
          { answer = answer,
            answerType = answerType,
            categoryName = categoryName,
            id = Kernel.Types.Id.Id id,
            question = question,
            rating = rating
          }

instance ToTType' Beam.FeedbackForm Domain.Types.FeedbackForm.FeedbackForm where
  toTType' (Domain.Types.FeedbackForm.FeedbackForm {..}) = do
    Beam.FeedbackFormT
      { Beam.answer = answer,
        Beam.answerType = answerType,
        Beam.categoryName = categoryName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.question = question,
        Beam.rating = rating
      }
