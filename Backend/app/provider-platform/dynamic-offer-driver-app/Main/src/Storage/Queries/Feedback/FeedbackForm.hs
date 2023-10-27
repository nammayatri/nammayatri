{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Feedback.FeedbackForm where

import Domain.Types.Feedback.FeedbackForm
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Feedback.FeedbackForm as BFF

findAllFeedback :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [FeedbackFormRes]
findAllFeedback = findAllWithDb [Se.Is BFF.id $ Se.Not $ Se.Eq ""]

findAllFeedbackByRating :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Int -> m [FeedbackFormRes]
findAllFeedbackByRating rating = findAllWithDb [Se.Or [Se.Is BFF.rating $ Se.Eq $ Just rating, Se.Is BFF.rating $ Se.Eq Nothing]]

instance FromTType' BFF.FeedbackForm FeedbackFormRes where
  fromTType' BFF.FeedbackFormT {..} = do
    pure $
      Just
        FeedbackFormRes
          { categoryName = categoryName,
            id = Id id,
            rating = rating,
            question = question,
            answer = answer,
            answerType = answerType
          }

instance ToTType' BFF.FeedbackForm FeedbackFormRes where
  toTType' FeedbackFormRes {..} =
    BFF.FeedbackFormT
      { BFF.categoryName = categoryName,
        BFF.id = getId id,
        BFF.rating = rating,
        BFF.question = question,
        BFF.answer = answer,
        BFF.answerType = answerType
      }
