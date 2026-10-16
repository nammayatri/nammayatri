{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RideFeedbackConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified IssueManagement.Common
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Feedback.Types
import qualified Tools.Beam.UtilsTH

data RideFeedbackConfig = RideFeedbackConfig
  { acknowledgement :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    actionRules :: Kernel.Prelude.Maybe [Lib.Feedback.Types.FeedbackActionRule],
    createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    displayTrigger :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackConfig.DisplayTrigger,
    enabled :: Kernel.Prelude.Bool,
    iconUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig,
    inputConfig :: Kernel.Prelude.Maybe Lib.Feedback.Types.InputConfig,
    isFollowUpOnly :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSkippable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    options :: Kernel.Prelude.Maybe [Lib.Feedback.Types.QuestionOption],
    priority :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    questionKey :: Kernel.Prelude.Text,
    questionType :: Lib.Feedback.Types.FeedbackQuestionType,
    stages :: Kernel.Prelude.Maybe [Domain.Types.RideFeedbackConfig.RideFeedbackStage],
    title :: [IssueManagement.Common.Translation],
    uiConfig :: Kernel.Prelude.Maybe Data.Aeson.Value,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DisplayTrigger = DisplayTrigger
  { autoDismissAfterSeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    showAfterSecondsFromAssign :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    showAfterSecondsFromBooking :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    showAfterSecondsFromRideStart :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RideFeedbackStage = BOOKING_CONFIRMED | AWAITING_REASSIGNMENT | RIDE_ASSIGNED | RIDE_IN_PROGRESS deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Bounded, (Enum))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RideFeedbackStage))
