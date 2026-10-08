-- | Wire types of the driver platform's during-ride feedback API (driver-app /internal/ride/{rideId}/feedback),
-- used by the calls in SharedLogic.CallBPPInternal. Kept apart because that module already has other
-- PENDING / FAILED constructors.
module SharedLogic.RideFeedback.Types where

import qualified API.Types.UI.RideFeedback as API
import qualified Data.Aeson as A
import Kernel.Prelude

-- | Mirrors driver-app's Domain.Types.RideFeedbackConfig.RideFeedbackActionType (separate packages):
-- a new action type must be added on both sides.
data RideFeedbackActionType
  = REPORT_ISSUE_TO_BPP
  | CREATE_TICKET
  | L0_SENSITIVE_WORD_CHECK
  | SLACK_ALERT
  | NOTIFY_RIDER
  | SAFETY_ESCALATION
  | -- | A type driver-app has and this rider-app does not yet (driver-app deployed first). Parsing it
    -- must not fail the call, since driver-app has already saved the answers; it is kept by name, so
    -- its result can be reported back and the action retried once rider-app supports it.
    UnknownActionType Text
  deriving (Eq, Show, Generic, ToSchema)

knownActionTypes :: [RideFeedbackActionType]
knownActionTypes = [REPORT_ISSUE_TO_BPP, CREATE_TICKET, L0_SENSITIVE_WORD_CHECK, SLACK_ALERT, NOTIFY_RIDER, SAFETY_ESCALATION]

actionTypeName :: RideFeedbackActionType -> Text
actionTypeName = \case
  UnknownActionType name -> name
  known -> show known

instance FromJSON RideFeedbackActionType where
  parseJSON = A.withText "RideFeedbackActionType" $ \name ->
    pure $ fromMaybe (UnknownActionType name) (find ((== name) . actionTypeName) knownActionTypes)

instance ToJSON RideFeedbackActionType where
  toJSON = A.String . actionTypeName

-- | Mirrors driver-app's Domain.Types.RideFeedbackResponse.RideFeedbackActionStatus.
data RideFeedbackActionStatus = PENDING | SUCCESS | FAILED
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data MatchedFeedbackAction = MatchedFeedbackAction
  { ruleId :: Text,
    actionType :: RideFeedbackActionType,
    params :: Maybe A.Value
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Mirrors driver-app's Domain.Types.RideFeedbackResponse.RideFeedbackActionResult.
data RideFeedbackActionResult = RideFeedbackActionResult
  { ruleId :: Text,
    actionType :: RideFeedbackActionType,
    status :: RideFeedbackActionStatus,
    attempts :: Int,
    externalRef :: Maybe Text,
    errorMessage :: Maybe Text,
    updatedAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | driver-app's questions response with each question left as JSON: rider-app decodes them one by one
-- and skips any it cannot read (e.g. a question type added on driver-app first), rather than failing
-- the whole fetch.
data QuestionsRes = QuestionsRes
  { serverTime :: UTCTime,
    questions :: [A.Value],
    followUpQuestions :: [A.Value]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

newtype SubmitFeedbackRes = SubmitFeedbackRes
  { results :: [SubmitFeedbackResult]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data SubmitFeedbackResult = SubmitFeedbackResult
  { questionId :: Text,
    accepted :: Bool,
    errorCode :: Maybe Text,
    acknowledgement :: Maybe Text,
    responseId :: Maybe Text,
    questionKey :: Maybe Text,
    actions :: [MatchedFeedbackAction]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

newtype ReportActionResultsReq = ReportActionResultsReq
  { results :: [RideFeedbackActionResult]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data RetryableActionsRes = RetryableActionsRes
  { responseId :: Text,
    questionKey :: Text,
    answer :: API.RideFeedbackAnswer,
    actions :: [MatchedFeedbackAction]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
