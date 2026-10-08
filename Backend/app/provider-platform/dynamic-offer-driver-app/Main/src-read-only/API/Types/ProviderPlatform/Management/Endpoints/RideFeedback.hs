{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.RideFeedback where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import qualified Domain.Types.Ride
import qualified Domain.Types.RideFeedbackConfig
import qualified Domain.Types.RideFeedbackResponse
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified IssueManagement.Common
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data QuestionEvaluation = QuestionEvaluation
  { configId :: Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig,
    eligible :: Kernel.Prelude.Bool,
    isFollowUp :: Kernel.Prelude.Bool,
    questionKey :: Data.Text.Text,
    reason :: Kernel.Prelude.Maybe Data.Text.Text,
    showAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideFeedbackMetaRes = RideFeedbackMetaRes {issueReportTypes :: [IssueManagement.Common.IssueReportType], supportedOperators :: [Data.Text.Text], uiLayouts :: [Data.Text.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideFeedbackPreviewRes = RideFeedbackPreviewRes
  { bookingId :: Data.Text.Text,
    context :: Data.Aeson.Value,
    evaluations :: [QuestionEvaluation],
    logicVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rideId :: Kernel.Prelude.Maybe Data.Text.Text,
    rideStatus :: Kernel.Prelude.Maybe Domain.Types.Ride.RideStatus,
    secondsSinceBooking :: Kernel.Prelude.Int,
    selectedQuestionKeys :: [Data.Text.Text],
    stage :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackConfig.RideFeedbackStage
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideFeedbackResponseItem = RideFeedbackResponseItem
  { actionResults :: Kernel.Prelude.Maybe [Domain.Types.RideFeedbackResponse.RideFeedbackActionResult],
    answer :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackResponse.RideFeedbackAnswer,
    answerScore :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    bookingId :: Data.Text.Text,
    configId :: Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig,
    configPilotVersions :: Kernel.Prelude.Maybe [Kernel.Prelude.Int],
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse,
    logicVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    questionKey :: Data.Text.Text,
    questionType :: Domain.Types.RideFeedbackConfig.RideFeedbackQuestionType,
    rideId :: Kernel.Prelude.Maybe Data.Text.Text,
    secondsIntoRide :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    secondsSinceBooking :: Kernel.Prelude.Int,
    stage :: Domain.Types.RideFeedbackConfig.RideFeedbackStage,
    status :: Domain.Types.RideFeedbackResponse.RideFeedbackResponseStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("rideFeedback" :> (GetRideFeedbackInspectPreview :<|> GetRideFeedbackInspectResponses :<|> GetRideFeedbackMeta))

type GetRideFeedbackInspectPreview = ("inspect" :> Capture "rideOrBookingId" Data.Text.Text :> "preview" :> Get ('[JSON]) RideFeedbackPreviewRes)

type GetRideFeedbackInspectResponses = ("inspect" :> Capture "rideOrBookingId" Data.Text.Text :> "responses" :> Get ('[JSON]) [RideFeedbackResponseItem])

type GetRideFeedbackMeta = ("meta" :> Get ('[JSON]) RideFeedbackMetaRes)

data RideFeedbackAPIs = RideFeedbackAPIs
  { getRideFeedbackInspectPreview :: (Data.Text.Text -> EulerHS.Types.EulerClient RideFeedbackPreviewRes),
    getRideFeedbackInspectResponses :: (Data.Text.Text -> EulerHS.Types.EulerClient [RideFeedbackResponseItem]),
    getRideFeedbackMeta :: (EulerHS.Types.EulerClient RideFeedbackMetaRes)
  }

mkRideFeedbackAPIs :: (Client EulerHS.Types.EulerClient API -> RideFeedbackAPIs)
mkRideFeedbackAPIs rideFeedbackClient = (RideFeedbackAPIs {..})
  where
    getRideFeedbackInspectPreview :<|> getRideFeedbackInspectResponses :<|> getRideFeedbackMeta = rideFeedbackClient

data RideFeedbackUserActionType
  = GET_RIDE_FEEDBACK_INSPECT_PREVIEW
  | GET_RIDE_FEEDBACK_INSPECT_RESPONSES
  | GET_RIDE_FEEDBACK_META
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''RideFeedbackUserActionType)])
