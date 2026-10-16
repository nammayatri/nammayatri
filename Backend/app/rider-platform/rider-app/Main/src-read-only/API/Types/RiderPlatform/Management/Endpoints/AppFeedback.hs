{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.AppFeedback where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.AppFeedbackConfig
import qualified Domain.Types.AppFeedbackResponse
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Lib.Feedback.Types
import Servant
import Servant.Client

data AppFeedbackEvaluation = AppFeedbackEvaluation
  { configId :: Kernel.Prelude.Text,
    eligible :: Kernel.Prelude.Bool,
    isFollowUp :: Kernel.Prelude.Bool,
    questionKey :: Kernel.Prelude.Text,
    reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AppFeedbackMetaRes = AppFeedbackMetaRes {placements :: [AppFeedbackPlacementInfo], supportedOperators :: [Kernel.Prelude.Text], uiLayouts :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AppFeedbackPlacementInfo = AppFeedbackPlacementInfo {placement :: Domain.Types.AppFeedbackConfig.AppFeedbackPlacement, subjectType :: Domain.Types.AppFeedbackResponse.AppFeedbackSubjectType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AppFeedbackPreviewRes = AppFeedbackPreviewRes
  { context :: Data.Aeson.Value,
    evaluations :: [AppFeedbackEvaluation],
    logicVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    nextAllowedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    placement :: Domain.Types.AppFeedbackConfig.AppFeedbackPlacement,
    selectedQuestionKeys :: [Kernel.Prelude.Text],
    subjectId :: Kernel.Prelude.Text,
    subjectType :: Domain.Types.AppFeedbackResponse.AppFeedbackSubjectType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AppFeedbackResponseRes = AppFeedbackResponseRes
  { actionResults :: Kernel.Prelude.Maybe [Lib.Feedback.Types.FeedbackActionResult],
    answer :: Kernel.Prelude.Maybe Lib.Feedback.Types.FeedbackAnswer,
    answerScore :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    configId :: Kernel.Prelude.Text,
    configPilotVersions :: Kernel.Prelude.Maybe [Kernel.Prelude.Int],
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Prelude.Text,
    logicVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    placement :: Domain.Types.AppFeedbackConfig.AppFeedbackPlacement,
    questionKey :: Kernel.Prelude.Text,
    questionType :: Lib.Feedback.Types.FeedbackQuestionType,
    status :: Lib.Feedback.Types.FeedbackResponseStatus,
    subjectId :: Kernel.Prelude.Text,
    subjectType :: Domain.Types.AppFeedbackResponse.AppFeedbackSubjectType,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("appFeedback" :> (GetAppFeedbackMeta :<|> GetAppFeedbackInspectPreview :<|> GetAppFeedbackInspectResponses :<|> PostAppFeedbackResponseRetryActions))

type GetAppFeedbackMeta = ("meta" :> Get ('[JSON]) AppFeedbackMetaRes)

type GetAppFeedbackInspectPreview =
  ( "inspect" :> Capture "customerId" Kernel.Prelude.Text :> "preview" :> QueryParam "subjectId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "placement"
           Domain.Types.AppFeedbackConfig.AppFeedbackPlacement
      :> Get ('[JSON]) AppFeedbackPreviewRes
  )

type GetAppFeedbackInspectResponses = ("inspect" :> Capture "customerId" Kernel.Prelude.Text :> "responses" :> Get ('[JSON]) [AppFeedbackResponseRes])

type PostAppFeedbackResponseRetryActions = ("response" :> Capture "responseId" Kernel.Prelude.Text :> "retryActions" :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data AppFeedbackAPIs = AppFeedbackAPIs
  { getAppFeedbackMeta :: (EulerHS.Types.EulerClient AppFeedbackMetaRes),
    getAppFeedbackInspectPreview :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Domain.Types.AppFeedbackConfig.AppFeedbackPlacement -> EulerHS.Types.EulerClient AppFeedbackPreviewRes),
    getAppFeedbackInspectResponses :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient [AppFeedbackResponseRes]),
    postAppFeedbackResponseRetryActions :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkAppFeedbackAPIs :: (Client EulerHS.Types.EulerClient API -> AppFeedbackAPIs)
mkAppFeedbackAPIs appFeedbackClient = (AppFeedbackAPIs {..})
  where
    getAppFeedbackMeta :<|> getAppFeedbackInspectPreview :<|> getAppFeedbackInspectResponses :<|> postAppFeedbackResponseRetryActions = appFeedbackClient

data AppFeedbackUserActionType
  = GET_APP_FEEDBACK_META
  | GET_APP_FEEDBACK_INSPECT_PREVIEW
  | GET_APP_FEEDBACK_INSPECT_RESPONSES
  | POST_APP_FEEDBACK_RESPONSE_RETRY_ACTIONS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''AppFeedbackUserActionType)])
