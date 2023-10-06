{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.Feedback.FeedbackForm where

import Domain.Types.Ride (Ride)
import Kernel.Prelude
import Kernel.Types.Id
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
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FeedbackFormItem = FeedbackFormItem
  { id :: Id FeedbackFormItem,
    rating :: Maybe Int,
    question :: Text,
    answer :: [Text],
    answerType :: AnswerType
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)

data AnswerType = Text | Checkbox | Radio
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FeedbackFormRes = FeedbackFormRes
  { categoryName :: Category,
    id :: Id FeedbackFormItem,
    rating :: Maybe Int,
    question :: Text,
    answer :: [Text],
    answerType :: AnswerType
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)

$(TH.mkBeamInstancesForEnum ''AnswerType)

$(TH.mkBeamInstancesForEnum ''Category)
