{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RideFeedbackConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Ride
import qualified IssueManagement.Common
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RideFeedbackConfig = RideFeedbackConfig
  { acknowledgement :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    actionRules :: Kernel.Prelude.Maybe [Domain.Types.RideFeedbackConfig.FeedbackActionRule],
    allowedRideStatuses :: Kernel.Prelude.Maybe [Domain.Types.Ride.RideStatus],
    createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    displayTrigger :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackConfig.DisplayTrigger,
    enabled :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig,
    inputConfig :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackConfig.InputConfig,
    isFollowUpOnly :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSkippable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    options :: Kernel.Prelude.Maybe [Domain.Types.RideFeedbackConfig.QuestionOption],
    priority :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    questionKey :: Kernel.Prelude.Text,
    questionType :: Domain.Types.RideFeedbackConfig.RideFeedbackQuestionType,
    title :: [IssueManagement.Common.Translation],
    uiConfig :: Kernel.Prelude.Maybe Data.Aeson.Value,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DisplayTrigger = DisplayTrigger
  { autoDismissAfterSeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    showAfterSecondsFromAssign :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    showAfterSecondsFromRideStart :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackAction = FeedbackAction {actionType :: Domain.Types.RideFeedbackConfig.RideFeedbackActionType, params :: Kernel.Prelude.Maybe Data.Aeson.Value}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackActionRule = FeedbackActionRule {actions :: [Domain.Types.RideFeedbackConfig.FeedbackAction], condition :: Kernel.Prelude.Maybe Data.Aeson.Value, ruleId :: Kernel.Prelude.Text}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data InputConfig = InputConfig
  { maxDurationSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxFiles :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxLabel :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    maxLength :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxSelections :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxValue :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    minLabel :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    minLength :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minSelections :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minValue :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    placeholder :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    step :: Kernel.Prelude.Maybe Kernel.Prelude.Double
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data QuestionOption = QuestionOption
  { iconUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isExclusive :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    key :: Kernel.Prelude.Text,
    label :: [IssueManagement.Common.Translation],
    nextQuestionKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    requiresText :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RideFeedbackActionType
  = REPORT_ISSUE_TO_BPP
  | CREATE_TICKET
  | L0_SENSITIVE_WORD_CHECK
  | SLACK_ALERT
  | NOTIFY_RIDER
  | SAFETY_ESCALATION
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Bounded, (Enum))

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
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Bounded, (Enum))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RideFeedbackActionType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RideFeedbackQuestionType))
