{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.RideFeedback where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data FeedbackInputConfigRes = FeedbackInputConfigRes
  { maxDurationSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxFiles :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxLabel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    maxLength :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxSelections :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxValue :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    minLabel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    minLength :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minSelections :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minValue :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    placeholder :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    step :: Kernel.Prelude.Maybe Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FeedbackOptionItem = FeedbackOptionItem
  { isExclusive :: Kernel.Prelude.Bool,
    key :: Kernel.Prelude.Text,
    label :: Kernel.Prelude.Text,
    nextQuestionKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    requiresText :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FeedbackQuestionItem = FeedbackQuestionItem
  { acknowledgement :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    autoDismissAfterSeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    inputConfig :: Kernel.Prelude.Maybe FeedbackInputConfigRes,
    isSkippable :: Kernel.Prelude.Bool,
    options :: Kernel.Prelude.Maybe [FeedbackOptionItem],
    questionId :: Kernel.Prelude.Text,
    questionKey :: Kernel.Prelude.Text,
    questionType :: RideFeedbackQuestionType,
    showAt :: Kernel.Prelude.UTCTime,
    title :: Kernel.Prelude.Text,
    uiConfig :: Kernel.Prelude.Maybe Data.Aeson.Value
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FeedbackResponseItem = FeedbackResponseItem {answer :: Kernel.Prelude.Maybe RideFeedbackAnswer, questionId :: Kernel.Prelude.Text, status :: RideFeedbackResponseStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FeedbackSubmitResult = FeedbackSubmitResult {accepted :: Kernel.Prelude.Bool, errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text, questionId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideFeedbackAnswer = RideFeedbackAnswer
  { mediaFileIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    number :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    selectedOptionKeys :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    text :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideFeedbackQuestionType
  = SINGLE_SELECT
  | MULTI_SELECT
  | YES_NO
  | STAR_RATING
  | EMOJI_SCALE
  | THUMBS
  | SCALE
  | TEXT_INPUT
  | NUMBER_INPUT
  | IMAGE_UPLOAD
  | AUDIO
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideFeedbackQuestionsRes = RideFeedbackQuestionsRes {followUpQuestions :: [FeedbackQuestionItem], questions :: [FeedbackQuestionItem], serverTime :: Kernel.Prelude.UTCTime}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideFeedbackResponseStatus
  = SHOWN
  | ANSWERED
  | SKIPPED
  | EXPIRED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubmitRideFeedbackReq = SubmitRideFeedbackReq {responses :: [FeedbackResponseItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubmitRideFeedbackRes = SubmitRideFeedbackRes {results :: [FeedbackSubmitResult]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
