{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.RideFeedback where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Ride
import qualified Domain.Types.RideFeedbackConfig
import qualified Domain.Types.RideFeedbackResponse
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
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
  { iconUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isExclusive :: Kernel.Prelude.Bool,
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
    priority :: Kernel.Prelude.Int,
    questionId :: Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig,
    questionKey :: Kernel.Prelude.Text,
    questionType :: Domain.Types.RideFeedbackConfig.RideFeedbackQuestionType,
    showAt :: Kernel.Prelude.UTCTime,
    title :: Kernel.Prelude.Text,
    uiConfig :: Kernel.Prelude.Maybe Data.Aeson.Value
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FeedbackResponseItem = FeedbackResponseItem
  { answer :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackResponse.RideFeedbackAnswer,
    clientTimestamp :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    location :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    parentQuestionId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig),
    questionId :: Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig,
    status :: Domain.Types.RideFeedbackResponse.RideFeedbackResponseStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FeedbackSubmitResult = FeedbackSubmitResult
  { accepted :: Kernel.Prelude.Bool,
    acknowledgement :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    questionId :: Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideFeedbackQuestionsRes = RideFeedbackQuestionsRes
  { followUpQuestions :: [FeedbackQuestionItem],
    nextPollAfterSeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    questions :: [FeedbackQuestionItem],
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    serverTime :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideFeedbackSubmittedRes = RideFeedbackSubmittedRes {responses :: [SubmittedFeedbackItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubmitRideFeedbackReq = SubmitRideFeedbackReq {responses :: [FeedbackResponseItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubmitRideFeedbackRes = SubmitRideFeedbackRes {results :: [FeedbackSubmitResult]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubmittedFeedbackItem = SubmittedFeedbackItem
  { answer :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackResponse.RideFeedbackAnswer,
    questionId :: Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig,
    questionKey :: Kernel.Prelude.Text,
    status :: Domain.Types.RideFeedbackResponse.RideFeedbackResponseStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
