{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AppFeedbackConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified IssueManagement.Common
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Lib.Feedback.Types
import qualified Tools.Beam.UtilsTH

data AppFeedbackConfig = AppFeedbackConfig
  { acknowledgement :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    actionRules :: Kernel.Prelude.Maybe [Lib.Feedback.Types.FeedbackActionRule],
    bypassCap :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    cooldownDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    displayTrigger :: Kernel.Prelude.Maybe Domain.Types.AppFeedbackConfig.AppFeedbackTrigger,
    enabled :: Kernel.Prelude.Bool,
    iconUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.AppFeedbackConfig.AppFeedbackConfig,
    inputConfig :: Kernel.Prelude.Maybe Lib.Feedback.Types.InputConfig,
    isFollowUpOnly :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSkippable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    options :: Kernel.Prelude.Maybe [Lib.Feedback.Types.QuestionOption],
    placements :: Kernel.Prelude.Maybe [Domain.Types.AppFeedbackConfig.AppFeedbackPlacement],
    priority :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    questionKey :: Kernel.Prelude.Text,
    questionType :: Lib.Feedback.Types.FeedbackQuestionType,
    subjectCondition :: Kernel.Prelude.Maybe Data.Aeson.Value,
    title :: [IssueManagement.Common.Translation],
    uiConfig :: Kernel.Prelude.Maybe Data.Aeson.Value,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data AppFeedbackPlacement = HOME | SEARCH_RESULTS | TICKET_BOOKED | JOURNEY_COMPLETED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Bounded, (Enum), (ToParamSchema))

data AppFeedbackTrigger = AppFeedbackTrigger {autoDismissAfterSeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int, showAfterSeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''AppFeedbackPlacement))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''AppFeedbackPlacement))
