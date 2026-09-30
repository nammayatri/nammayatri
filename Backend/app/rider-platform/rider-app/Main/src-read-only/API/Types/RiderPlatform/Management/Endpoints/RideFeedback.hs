{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.RideFeedback where

import qualified Dashboard.Common
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import qualified Domain.Types.RideFeedbackConfig
import qualified Domain.Types.RideFeedbackResponse
import qualified "beckn-spec" Domain.Types.RideStatus
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified IssueManagement.Common
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data CloneRideFeedbackConfigReq = CloneRideFeedbackConfigReq {enabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, targetCities :: [Kernel.Types.Beckn.Context.City]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CloneRideFeedbackConfigReq where
  hideSecrets = Kernel.Prelude.identity

data CloneRideFeedbackConfigRes = CloneRideFeedbackConfigRes {created :: [ClonedConfig], skipped :: [SkippedClone]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ClonedConfig = ClonedConfig {city :: Kernel.Types.Beckn.Context.City, configId :: Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateRideFeedbackConfigReq = CreateRideFeedbackConfigReq
  { acknowledgement :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    actionRules :: Kernel.Prelude.Maybe [Domain.Types.RideFeedbackConfig.FeedbackActionRule],
    allowedRideStatuses :: Kernel.Prelude.Maybe [Domain.Types.RideStatus.RideStatus],
    cooldownDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    description :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    displayTrigger :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackConfig.DisplayTrigger,
    enabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    endsAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    inputConfig :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackConfig.InputConfig,
    isFollowUpOnly :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSkippable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    maxShowsPerRide :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    options :: Kernel.Prelude.Maybe [Domain.Types.RideFeedbackConfig.QuestionOption],
    priority :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    questionKey :: Data.Text.Text,
    questionType :: Domain.Types.RideFeedbackConfig.RideFeedbackQuestionType,
    startsAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    title :: [IssueManagement.Common.Translation],
    uiConfig :: Kernel.Prelude.Maybe Data.Aeson.Value
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateRideFeedbackConfigReq where
  hideSecrets = Kernel.Prelude.identity

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

data RideFeedbackConfigItem = RideFeedbackConfigItem
  { acknowledgement :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    actionRules :: Kernel.Prelude.Maybe [Domain.Types.RideFeedbackConfig.FeedbackActionRule],
    allowedRideStatuses :: Kernel.Prelude.Maybe [Domain.Types.RideStatus.RideStatus],
    cooldownDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    displayTrigger :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackConfig.DisplayTrigger,
    enabled :: Kernel.Prelude.Bool,
    endsAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig,
    inputConfig :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackConfig.InputConfig,
    isFollowUpOnly :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSkippable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    maxShowsPerRide :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    options :: Kernel.Prelude.Maybe [Domain.Types.RideFeedbackConfig.QuestionOption],
    priority :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    questionKey :: Data.Text.Text,
    questionType :: Domain.Types.RideFeedbackConfig.RideFeedbackQuestionType,
    startsAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    title :: [IssueManagement.Common.Translation],
    uiConfig :: Kernel.Prelude.Maybe Data.Aeson.Value,
    updatedAt :: Kernel.Prelude.UTCTime,
    version :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideFeedbackConfigListRes = RideFeedbackConfigListRes {configs :: [RideFeedbackConfigItem], summary :: Dashboard.Common.Summary, totalItems :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideFeedbackConfigUpsertRes = RideFeedbackConfigUpsertRes {id :: Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig, version :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideFeedbackMetaRes = RideFeedbackMetaRes
  { actionTypes :: [Domain.Types.RideFeedbackConfig.RideFeedbackActionType],
    allowedRideStatuses :: [Domain.Types.RideStatus.RideStatus],
    contextSample :: Data.Aeson.Value,
    deliveryModes :: [Domain.Types.RideFeedbackConfig.RideFeedbackDeliveryMode],
    issueReportTypes :: [IssueManagement.Common.IssueReportType],
    languages :: [Kernel.External.Types.Language],
    questionTypes :: [Domain.Types.RideFeedbackConfig.RideFeedbackQuestionType],
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
    rideStatus :: Domain.Types.RideStatus.RideStatus,
    selectedQuestionKeys :: [Data.Text.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideFeedbackResponseItem = RideFeedbackResponseItem
  { actionResults :: Kernel.Prelude.Maybe [Domain.Types.RideFeedbackResponse.RideFeedbackActionResult],
    answer :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackResponse.RideFeedbackAnswer,
    configId :: Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig,
    configVersion :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse,
    logicVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    parentResponseId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse),
    questionKey :: Data.Text.Text,
    rideStatusAtResponse :: Kernel.Prelude.Maybe Domain.Types.RideStatus.RideStatus,
    secondsIntoRide :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    status :: Domain.Types.RideFeedbackResponse.RideFeedbackResponseStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SkippedClone = SkippedClone {city :: Kernel.Types.Beckn.Context.City, reason :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ToggleRideFeedbackConfigReq = ToggleRideFeedbackConfigReq {enabled :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ToggleRideFeedbackConfigReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateRideFeedbackConfigReq = UpdateRideFeedbackConfigReq
  { acknowledgement :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    actionRules :: Kernel.Prelude.Maybe [Domain.Types.RideFeedbackConfig.FeedbackActionRule],
    allowedRideStatuses :: Kernel.Prelude.Maybe [Domain.Types.RideStatus.RideStatus],
    cooldownDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    description :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    displayTrigger :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackConfig.DisplayTrigger,
    endsAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    inputConfig :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackConfig.InputConfig,
    isFollowUpOnly :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSkippable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    maxShowsPerRide :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    options :: Kernel.Prelude.Maybe [Domain.Types.RideFeedbackConfig.QuestionOption],
    priority :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    questionType :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackConfig.RideFeedbackQuestionType,
    startsAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    title :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    uiConfig :: Kernel.Prelude.Maybe Data.Aeson.Value
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateRideFeedbackConfigReq where
  hideSecrets = Kernel.Prelude.identity

data ValidateRideFeedbackConfigRes = ValidateRideFeedbackConfigRes {errors :: [Data.Text.Text], isValid :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("rideFeedback" :> (GetRideFeedbackConfigList :<|> GetRideFeedbackConfig :<|> PostRideFeedbackConfigCreate :<|> PostRideFeedbackConfigUpdate :<|> PostRideFeedbackConfigToggle :<|> PostRideFeedbackConfigClone :<|> PostRideFeedbackConfigValidate :<|> GetRideFeedbackRidePreview :<|> GetRideFeedbackRideResponses :<|> PostRideFeedbackResponseRetryActions :<|> GetRideFeedbackMeta))

type GetRideFeedbackConfigList =
  ( "config" :> "list" :> QueryParam "questionKey" Data.Text.Text :> QueryParam "enabled" Kernel.Prelude.Bool
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> Get ('[JSON]) RideFeedbackConfigListRes
  )

type GetRideFeedbackConfig = ("config" :> Capture "configId" (Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig) :> Get ('[JSON]) RideFeedbackConfigItem)

type PostRideFeedbackConfigCreate = ("config" :> "create" :> ReqBody ('[JSON]) CreateRideFeedbackConfigReq :> Post ('[JSON]) RideFeedbackConfigUpsertRes)

type PostRideFeedbackConfigUpdate =
  ( "config" :> Capture "configId" (Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig) :> "update"
      :> ReqBody
           ('[JSON])
           UpdateRideFeedbackConfigReq
      :> Post ('[JSON]) RideFeedbackConfigUpsertRes
  )

type PostRideFeedbackConfigToggle =
  ( "config" :> Capture "configId" (Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig) :> "toggle"
      :> ReqBody
           ('[JSON])
           ToggleRideFeedbackConfigReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type PostRideFeedbackConfigClone =
  ( "config" :> Capture "configId" (Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig) :> "clone"
      :> ReqBody
           ('[JSON])
           CloneRideFeedbackConfigReq
      :> Post ('[JSON]) CloneRideFeedbackConfigRes
  )

type PostRideFeedbackConfigValidate = ("config" :> "validate" :> ReqBody ('[JSON]) CreateRideFeedbackConfigReq :> Post ('[JSON]) ValidateRideFeedbackConfigRes)

type GetRideFeedbackRidePreview = ("ride" :> Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "preview" :> Get ('[JSON]) RideFeedbackPreviewRes)

type GetRideFeedbackRideResponses = ("ride" :> Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "responses" :> Get ('[JSON]) [RideFeedbackResponseItem])

type PostRideFeedbackResponseRetryActions =
  ( "response" :> Capture "responseId" (Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse) :> "retryActions"
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type GetRideFeedbackMeta = ("meta" :> Get ('[JSON]) RideFeedbackMetaRes)

data RideFeedbackAPIs = RideFeedbackAPIs
  { getRideFeedbackConfigList :: (Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient RideFeedbackConfigListRes),
    getRideFeedbackConfig :: (Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> EulerHS.Types.EulerClient RideFeedbackConfigItem),
    postRideFeedbackConfigCreate :: (CreateRideFeedbackConfigReq -> EulerHS.Types.EulerClient RideFeedbackConfigUpsertRes),
    postRideFeedbackConfigUpdate :: (Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> UpdateRideFeedbackConfigReq -> EulerHS.Types.EulerClient RideFeedbackConfigUpsertRes),
    postRideFeedbackConfigToggle :: (Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> ToggleRideFeedbackConfigReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postRideFeedbackConfigClone :: (Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> CloneRideFeedbackConfigReq -> EulerHS.Types.EulerClient CloneRideFeedbackConfigRes),
    postRideFeedbackConfigValidate :: (CreateRideFeedbackConfigReq -> EulerHS.Types.EulerClient ValidateRideFeedbackConfigRes),
    getRideFeedbackRidePreview :: (Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient RideFeedbackPreviewRes),
    getRideFeedbackRideResponses :: (Kernel.Types.Id.Id Dashboard.Common.Ride -> EulerHS.Types.EulerClient [RideFeedbackResponseItem]),
    postRideFeedbackResponseRetryActions :: (Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getRideFeedbackMeta :: (EulerHS.Types.EulerClient RideFeedbackMetaRes)
  }

mkRideFeedbackAPIs :: (Client EulerHS.Types.EulerClient API -> RideFeedbackAPIs)
mkRideFeedbackAPIs rideFeedbackClient = (RideFeedbackAPIs {..})
  where
    getRideFeedbackConfigList :<|> getRideFeedbackConfig :<|> postRideFeedbackConfigCreate :<|> postRideFeedbackConfigUpdate :<|> postRideFeedbackConfigToggle :<|> postRideFeedbackConfigClone :<|> postRideFeedbackConfigValidate :<|> getRideFeedbackRidePreview :<|> getRideFeedbackRideResponses :<|> postRideFeedbackResponseRetryActions :<|> getRideFeedbackMeta = rideFeedbackClient

data RideFeedbackUserActionType
  = GET_RIDE_FEEDBACK_CONFIG_LIST
  | GET_RIDE_FEEDBACK_CONFIG
  | POST_RIDE_FEEDBACK_CONFIG_CREATE
  | POST_RIDE_FEEDBACK_CONFIG_UPDATE
  | POST_RIDE_FEEDBACK_CONFIG_TOGGLE
  | POST_RIDE_FEEDBACK_CONFIG_CLONE
  | POST_RIDE_FEEDBACK_CONFIG_VALIDATE
  | GET_RIDE_FEEDBACK_RIDE_PREVIEW
  | GET_RIDE_FEEDBACK_RIDE_RESPONSES
  | POST_RIDE_FEEDBACK_RESPONSE_RETRY_ACTIONS
  | GET_RIDE_FEEDBACK_META
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''RideFeedbackUserActionType)])
