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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FeedbackForm where

import qualified Domain.Types.FeedbackForm as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

derivePersistField "Domain.Category"
derivePersistField "Domain.AnswerType"

deriving instance Read Domain.Category

deriving instance Read Domain.AnswerType

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FeedbackFormT sql=feedback_form
      id Text
      categoryName Domain.Category
      rating Int Maybe
      question Text
      answer (PostgresList Text)
      answerType Domain.AnswerType
      Primary id
      deriving Generic
    |]

instance TEntityKey FeedbackFormT where
  type DomainKey FeedbackFormT = Id Domain.FeedbackFormRes
  fromKey (FeedbackFormTKey _id) = Id _id
  toKey (Id id) = FeedbackFormTKey id

instance FromTType FeedbackFormT Domain.FeedbackFormRes where
  fromTType FeedbackFormT {..} = do
    return $
      Domain.FeedbackFormRes
        { id = Id id,
          answer = unPostgresList answer,
          ..
        }

instance ToTType FeedbackFormT Domain.FeedbackFormRes where
  toTType Domain.FeedbackFormRes {..} =
    FeedbackFormT
      { id = getId id,
        answer = PostgresList answer,
        ..
      }
