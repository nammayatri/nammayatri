{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.AppFeedback where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.AppFeedbackConfig
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Lib.Feedback.Types
import qualified Lib.Feedback.View
import Servant
import Tools.Auth

data AppFeedbackManifestItem = AppFeedbackManifestItem
  { bypassCap :: Kernel.Prelude.Bool,
    checksSubject :: Kernel.Prelude.Bool,
    placements :: [Domain.Types.AppFeedbackConfig.AppFeedbackPlacement],
    question :: Lib.Feedback.View.FeedbackQuestionItem,
    showAfterSeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AppFeedbackManifestRes = AppFeedbackManifestRes
  { followUpQuestions :: [Lib.Feedback.View.FeedbackQuestionItem],
    nextAllowedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    questions :: [AppFeedbackManifestItem],
    serverTime :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AppFeedbackResponseItem = AppFeedbackResponseItem {answer :: Kernel.Prelude.Maybe Lib.Feedback.Types.FeedbackAnswer, questionId :: Kernel.Prelude.Text, status :: Lib.Feedback.Types.FeedbackResponseStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AppFeedbackSubmitReq = AppFeedbackSubmitReq {placement :: Domain.Types.AppFeedbackConfig.AppFeedbackPlacement, responses :: [AppFeedbackResponseItem], subjectId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AppFeedbackSubmitRes = AppFeedbackSubmitRes {nextAllowedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime, results :: [AppFeedbackSubmitResult]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AppFeedbackSubmitResult = AppFeedbackSubmitResult {accepted :: Kernel.Prelude.Bool, errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text, questionId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
