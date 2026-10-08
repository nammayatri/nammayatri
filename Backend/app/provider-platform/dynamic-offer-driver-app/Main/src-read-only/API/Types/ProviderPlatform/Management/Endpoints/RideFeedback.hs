{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.RideFeedback where

import qualified Dashboard.Common
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

data RideFeedbackMetaRes = RideFeedbackMetaRes
  { allowedRideStatuses :: [Domain.Types.Ride.RideStatus],
    issueReportTypes :: [IssueManagement.Common.IssueReportType],
    supportedOperators :: [Data.Text.Text],
    uiLayouts :: [Data.Text.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideFeedbackPreviewRes = RideFeedbackPreviewRes
  { context :: Data.Aeson.Value,
    evaluations :: [QuestionEvaluation],
    logicVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride,
    rideStatus :: Domain.Types.Ride.RideStatus,
    selectedQuestionKeys :: [Data.Text.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideFeedbackResponseItem = RideFeedbackResponseItem
  { actionResults :: Kernel.Prelude.Maybe [Domain.Types.RideFeedbackResponse.RideFeedbackActionResult],
    answer :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackResponse.RideFeedbackAnswer,
    configId :: Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig,
    configPilotVersions :: Kernel.Prelude.Maybe [Kernel.Prelude.Int],
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse,
    logicVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    parentResponseId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse),
    questionKey :: Data.Text.Text,
    rideStatusAtResponse :: Kernel.Prelude.Maybe Domain.Types.Ride.RideStatus,
    secondsIntoRide :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    status :: Domain.Types.RideFeedbackResponse.RideFeedbackResponseStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("rideFeedback" :> (GetRideFeedbackRidePreview :<|> GetRideFeedbackRideResponses :<|> GetRideFeedbackMeta))

type GetRideFeedbackRidePreview = ("ride" :> Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "preview" :> Get ('[JSON]) RideFeedbackPreviewRes)

type GetRideFeedbackRideResponses = ("ride" :> Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "responses" :> Get ('[JSON]) [RideFeedbackResponseItem])

type GetRideFeedbackMeta = ("meta" :> Get ('[JSON]) RideFeedbackMetaRes)

data RideFeedbackAPIs = RideFeedbackAPIs
  { getRideFeedbackRidePreview :: (Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient RideFeedbackPreviewRes),
    getRideFeedbackRideResponses :: (Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient [RideFeedbackResponseItem]),
    getRideFeedbackMeta :: (EulerHS.Types.EulerClient RideFeedbackMetaRes)
  }

mkRideFeedbackAPIs :: (Client EulerHS.Types.EulerClient API -> RideFeedbackAPIs)
mkRideFeedbackAPIs rideFeedbackClient = (RideFeedbackAPIs {..})
  where
    getRideFeedbackRidePreview :<|> getRideFeedbackRideResponses :<|> getRideFeedbackMeta = rideFeedbackClient

data RideFeedbackUserActionType
  = GET_RIDE_FEEDBACK_RIDE_PREVIEW
  | GET_RIDE_FEEDBACK_RIDE_RESPONSES
  | GET_RIDE_FEEDBACK_META
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''RideFeedbackUserActionType)])
