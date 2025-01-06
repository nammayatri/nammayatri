{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FeedbackForm (module Domain.Types.FeedbackForm, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.FeedbackForm as ReExport
import qualified Domain.Types.Ride
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FeedbackForm = FeedbackForm
  { answer :: [Kernel.Prelude.Text],
    answerType :: Domain.Types.FeedbackForm.AnswerType,
    categoryName :: Domain.Types.FeedbackForm.Category,
    id :: Kernel.Types.Id.Id Domain.Types.FeedbackForm.FeedbackFormItem,
    question :: Kernel.Prelude.Text,
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)

data AnswerType = Text | Checkbox | Radio deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data Category = RIDE | DRIVER | VEHICLE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data FeedbackAnswer = FeedbackAnswer {answer :: [Kernel.Prelude.Text], questionId :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackFormAPIEntity = FeedbackFormAPIEntity {categoryName :: Domain.Types.FeedbackForm.Category, feedbackForm :: [Domain.Types.FeedbackForm.FeedbackFormItem]}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data FeedbackFormItem = FeedbackFormItem
  { answer :: [Kernel.Prelude.Text],
    answerType :: Domain.Types.FeedbackForm.AnswerType,
    id :: Kernel.Types.Id.Id Domain.Types.FeedbackForm.FeedbackFormItem,
    question :: Kernel.Prelude.Text,
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

newtype FeedbackFormList = FeedbackFormList {_data :: [Domain.Types.FeedbackForm.FeedbackFormAPIEntity]} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data FeedbackFormReq = FeedbackFormReq {feedback :: [Domain.Types.FeedbackForm.FeedbackAnswer], rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''AnswerType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''Category)
