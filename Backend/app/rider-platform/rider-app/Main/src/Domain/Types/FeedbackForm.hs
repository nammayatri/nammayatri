{-# LANGUAGE TemplateHaskell #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
--TODO remove
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Domain.Types.FeedbackForm where

import Data.List (groupBy)
import Domain.Types.Ride (Ride)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Dhall (FromDhall)
import qualified Tools.Beam.UtilsTH as TH

data FeedbackFormReq = FeedbackFormReq
  { rideId :: Id Ride,
    feedback :: [FeedbackAnswer]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data FeedbackAnswer = FeedbackAnswer
  { questionId :: Text,
    answer :: [Text]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype FeedbackFormList = FeedbackFormList {_data :: [FeedbackFormAPIEntity]}
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)

data FeedbackFormAPIEntity = FeedbackFormAPIEntity
  { categoryName :: Category,
    feedbackForm :: [FeedbackFormItem]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)

data Category = RIDE | DRIVER | VEHICLE
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq, Read, Ord)

data FeedbackFormItem = FeedbackFormItem
  { id :: Id FeedbackFormItem,
    rating :: Maybe Int,
    question :: Text,
    answer :: [Text],
    answerType :: AnswerType
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)

data AnswerType = Text | Checkbox | Radio
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq, Read, Ord)

data FeedbackFormRes = FeedbackFormRes
  { categoryName :: Category,
    id :: Id FeedbackFormItem,
    rating :: Maybe Int,
    question :: Text,
    answer :: [Text],
    answerType :: AnswerType
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)

newtype CacheFeedbackFormConfig = CacheFeedbackFormConfig
  { configsExpTime :: Seconds
  }
  deriving (Generic, FromDhall)

type HasCacheFeedbackFormConfig r = HasField "cacheFeedbackFormConfig" r CacheFeedbackFormConfig

makeFeedbackFormList :: [FeedbackFormAPIEntity] -> FeedbackFormList
makeFeedbackFormList item =
  FeedbackFormList
    { _data = item
    }

makeFeedbackFormAPIEntity :: [FeedbackFormRes] -> [FeedbackFormAPIEntity]
makeFeedbackFormAPIEntity response = map convertGroup groupedEntities
  where
    groupedEntities = groupBy (\a b -> a.categoryName == b.categoryName) response
    convertGroup :: [FeedbackFormRes] -> FeedbackFormAPIEntity
    convertGroup [] = FeedbackFormAPIEntity {}
    convertGroup group@(res : _) =
      FeedbackFormAPIEntity
        { categoryName = res.categoryName,
          feedbackForm = map convertResponse group
        }
    convertResponse :: FeedbackFormRes -> FeedbackFormItem
    convertResponse res =
      FeedbackFormItem
        { id = res.id,
          rating = res.rating,
          question = res.question,
          answer = res.answer,
          answerType = res.answerType
        }

$(TH.mkBeamInstancesForEnum ''Category)

$(TH.mkBeamInstancesForEnum ''AnswerType)
