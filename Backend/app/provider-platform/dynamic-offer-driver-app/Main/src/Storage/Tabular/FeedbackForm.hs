{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FeedbackForm where

import Beckn.Types.Core.Taxi.Rating.FeedbackForm (AnswerType)
import qualified Domain.Types.FeedbackForm as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Storage.Tabular.RatingCategories as RC

derivePersistField "AnswerType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FeedbackFormT sql=feedback_form
      id Text
      categoryId RC.RatingCategoryTId
      ratingValue Int
      question Text
      answerType AnswerType
      Primary id
      deriving Generic
    |]

instance TEntityKey FeedbackFormT where
  type DomainKey FeedbackFormT = Id Domain.FeedbackForm
  fromKey (FeedbackFormTKey _id) = Id _id
  toKey (Id id) = FeedbackFormTKey id

instance FromTType FeedbackFormT Domain.FeedbackForm where
  fromTType FeedbackFormT {..} = do
    return $
      Domain.FeedbackForm
        { id = Id id,
          categoryId = fromKey categoryId,
          answer_type = answerType,
          ..
        }

instance ToTType FeedbackFormT Domain.FeedbackForm where
  toTType Domain.FeedbackForm {..} =
    FeedbackFormT
      { id = getId id,
        categoryId = toKey categoryId,
        answerType = answer_type,
        ..
      }
