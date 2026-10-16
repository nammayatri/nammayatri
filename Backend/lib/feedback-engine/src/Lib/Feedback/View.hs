-- | A question as the rider app shows it: texts picked for the rider's language, defaults filled in.
module Lib.Feedback.View
  ( FeedbackQuestionItem (..),
    FeedbackOptionItem (..),
    FeedbackInputConfigRes (..),
    QuestionTiming (..),
    mkQuestionItem,
    uiLayouts,
  )
where

import qualified Data.Aeson as A
import Kernel.External.Types (Language)
import Kernel.Prelude
import Lib.Feedback.Question (FeedbackQuestion, pickTranslation)
import Lib.Feedback.Types

data FeedbackQuestionItem = FeedbackQuestionItem
  { questionId :: Text,
    questionKey :: Text,
    questionType :: FeedbackQuestionType,
    title :: Text,
    description :: Maybe Text,
    options :: Maybe [FeedbackOptionItem],
    inputConfig :: Maybe FeedbackInputConfigRes,
    acknowledgement :: Maybe Text,
    iconUrl :: Maybe Text,
    isSkippable :: Bool,
    showAt :: UTCTime,
    autoDismissAfterSeconds :: Maybe Int,
    uiConfig :: Maybe A.Value
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackOptionItem = FeedbackOptionItem
  { key :: Text,
    label :: Text,
    iconUrl :: Maybe Text,
    nextQuestionKey :: Maybe Text,
    requiresText :: Bool,
    isExclusive :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackInputConfigRes = FeedbackInputConfigRes
  { placeholder :: Maybe Text,
    minLength :: Maybe Int,
    maxLength :: Maybe Int,
    minValue :: Maybe Double,
    maxValue :: Maybe Double,
    step :: Maybe Double,
    minSelections :: Maybe Int,
    maxSelections :: Maybe Int,
    maxFiles :: Maybe Int,
    maxDurationSec :: Maybe Int,
    minLabel :: Maybe Text,
    maxLabel :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | When the app shows the question and for how long; each platform works it out from its own trigger.
data QuestionTiming = QuestionTiming
  { showAt :: UTCTime,
    autoDismissAfterSeconds :: Maybe Int
  }

mkQuestionItem ::
  (FeedbackQuestion q, HasField "isSkippable" q (Maybe Bool)) =>
  -- | The question's id, as the app sends it back with its response.
  Text ->
  Language ->
  QuestionTiming ->
  q ->
  FeedbackQuestionItem
mkQuestionItem questionId language timing config =
  FeedbackQuestionItem
    { questionId,
      questionKey = config.questionKey,
      questionType = config.questionType,
      title = fromMaybe config.questionKey (pick $ Just config.title),
      description = pick config.description,
      options = map mkOption <$> config.options,
      inputConfig = mkInputConfig <$> config.inputConfig,
      acknowledgement = pick config.acknowledgement,
      iconUrl = config.iconUrl,
      isSkippable = fromMaybe True config.isSkippable,
      showAt = timing.showAt,
      autoDismissAfterSeconds = timing.autoDismissAfterSeconds,
      uiConfig = config.uiConfig
    }
  where
    pick = (>>= pickTranslation language)
    mkOption o =
      FeedbackOptionItem
        { key = o.key,
          label = fromMaybe o.key (pickTranslation language o.label),
          iconUrl = o.iconUrl,
          nextQuestionKey = o.nextQuestionKey,
          requiresText = fromMaybe False o.requiresText,
          isExclusive = fromMaybe False o.isExclusive
        }
    mkInputConfig ic =
      FeedbackInputConfigRes
        { placeholder = pick ic.placeholder,
          minLength = ic.minLength,
          maxLength = ic.maxLength,
          minValue = ic.minValue,
          maxValue = ic.maxValue,
          step = ic.step,
          minSelections = ic.minSelections,
          maxSelections = ic.maxSelections,
          maxFiles = ic.maxFiles,
          maxDurationSec = ic.maxDurationSec,
          minLabel = pick ic.minLabel,
          maxLabel = pick ic.maxLabel
        }

-- | Layouts the rider app knows for a question card (uiConfig.layout); the dashboard offers these.
uiLayouts :: [Text]
uiLayouts = ["inline_card", "bottom_sheet", "chips", "poll"]
