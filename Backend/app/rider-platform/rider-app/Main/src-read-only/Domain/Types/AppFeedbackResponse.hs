{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AppFeedbackResponse where

import Data.Aeson
import qualified Domain.Types.AppFeedbackConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Feedback.Types
import qualified Tools.Beam.UtilsTH

data AppFeedbackResponse = AppFeedbackResponse
  { actionResults :: Kernel.Prelude.Maybe [Lib.Feedback.Types.FeedbackActionResult],
    answer :: Kernel.Prelude.Maybe Lib.Feedback.Types.FeedbackAnswer,
    answerScore :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    configId :: Kernel.Types.Id.Id Domain.Types.AppFeedbackConfig.AppFeedbackConfig,
    configPilotVersions :: Kernel.Prelude.Maybe [Kernel.Prelude.Int],
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.AppFeedbackResponse.AppFeedbackResponse,
    logicVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    placement :: Domain.Types.AppFeedbackConfig.AppFeedbackPlacement,
    questionKey :: Kernel.Prelude.Text,
    questionType :: Lib.Feedback.Types.FeedbackQuestionType,
    status :: Lib.Feedback.Types.FeedbackResponseStatus,
    subjectId :: Kernel.Prelude.Text,
    subjectType :: Domain.Types.AppFeedbackResponse.AppFeedbackSubjectType,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data AppFeedbackSubjectType = RIDER | FRFS_BOOKING | JOURNEY deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Bounded, (Enum))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''AppFeedbackSubjectType))
