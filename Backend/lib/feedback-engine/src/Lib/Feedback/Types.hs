{-# OPTIONS_GHC -Wno-orphans #-}

-- | Question and answer types shared by ride feedback (driver platform, ride_feedback_config) and app
-- feedback (rider platform, app_feedback_config). Both store these as JSON or text columns, so field and
-- constructor names are part of the stored format: rename only with a data migration.
module Lib.Feedback.Types where

import Data.Aeson (Value)
import IssueManagement.Common (Translation)
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Prelude

data FeedbackQuestionType
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
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Bounded, Enum)

-- | What an answer can trigger. Ride-only types (needing a ride or its driver) are refused on questions
-- without one; rider-app runs all of them (SharedLogic.RideFeedback.Actions).
data FeedbackActionType
  = REPORT_ISSUE_TO_BPP
  | CREATE_TICKET
  | L0_SENSITIVE_WORD_CHECK
  | SLACK_ALERT
  | NOTIFY_RIDER
  | SAFETY_ESCALATION
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Bounded, Enum)

data QuestionOption = QuestionOption
  { key :: Text,
    label :: [Translation],
    iconUrl :: Maybe Text,
    nextQuestionKey :: Maybe Text,
    requiresText :: Maybe Bool,
    isExclusive :: Maybe Bool,
    -- | 0 (bad) to 1 (good): the answer_score of picking it; not set = not scored.
    score :: Maybe Double
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data InputConfig = InputConfig
  { placeholder :: Maybe [Translation],
    minLength :: Maybe Int,
    maxLength :: Maybe Int,
    minValue :: Maybe Double,
    maxValue :: Maybe Double,
    step :: Maybe Double,
    minSelections :: Maybe Int,
    maxSelections :: Maybe Int,
    maxFiles :: Maybe Int,
    maxDurationSec :: Maybe Int,
    minLabel :: Maybe [Translation],
    maxLabel :: Maybe [Translation]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackAction = FeedbackAction
  { actionType :: FeedbackActionType,
    params :: Maybe Value
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackActionRule = FeedbackActionRule
  { ruleId :: Text,
    condition :: Maybe Value,
    actions :: [FeedbackAction]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackResponseStatus = SHOWN | ANSWERED | SKIPPED | EXPIRED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Bounded, Enum)

-- | Each question type fills only its own part (options / text / number / rating / media).
data FeedbackAnswer = FeedbackAnswer
  { selectedOptionKeys :: Maybe [Text],
    text :: Maybe Text,
    number :: Maybe Double,
    rating :: Maybe Int,
    mediaFileIds :: Maybe [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackActionStatus = PENDING | SUCCESS | FAILED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Bounded, Enum)

data FeedbackActionResult = FeedbackActionResult
  { ruleId :: Text,
    actionType :: FeedbackActionType,
    status :: FeedbackActionStatus,
    attempts :: Int,
    externalRef :: Maybe Text,
    errorMessage :: Maybe Text,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | An action whose rule matched an answer, to be run.
data MatchedFeedbackAction = MatchedFeedbackAction
  { ruleId :: Text,
    actionType :: FeedbackActionType,
    params :: Maybe Value
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''FeedbackQuestionType)

$(mkBeamInstancesForEnumAndList ''FeedbackActionType)

$(mkBeamInstancesForEnumAndList ''FeedbackResponseStatus)

$(mkBeamInstancesForEnumAndList ''FeedbackActionStatus)
