{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.MerchantOnboarding where

import qualified Dashboard.Common
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Types.MerchantOnboarding
import qualified "this" Domain.Types.MerchantOnboarding.Handler
import qualified "this" Domain.Types.MerchantOnboardingStep
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client
import qualified Storage.Types

data ApproveResponse = ApproveResponse {success :: Kernel.Prelude.Bool, handler :: Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.Handler.DashboardSideHandler}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UploadFileRequest = UploadFileRequest {file :: Kernel.Prelude.FilePath, reqContentType :: Kernel.Prelude.Text, fileType :: Storage.Types.FileType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UploadFileRequest where
  hideSecrets = Kernel.Prelude.identity

newtype UploadFileResponse = UploadFileResponse {fileId :: Kernel.Types.Id.Id Dashboard.Common.File}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = (MerchantOnboardingInfo :<|> MerchantOnboardingStart :<|> MerchantOnboardingList :<|> MerchantOnboardingStepSubmit :<|> MerchantOnboardingStepUpdatePayload :<|> MerchantOnboardingStepReject :<|> MerchantOnboardingStepApprove :<|> MerchantOnboardingStepUploadFile :<|> MerchantOnboardingReject :<|> MerchantOnboadingListAll :<|> MerchantOnboardingStepList :<|> MerchantOnboardingGetFile :<|> MerchantOnboardingCancel)

type MerchantOnboardingInfo =
  ( "onboarding" :> Capture "onboardingType" Kernel.Prelude.Text :> "info" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> Get '[JSON] Domain.Types.MerchantOnboarding.MerchantOnboardingAPI
  )

type MerchantOnboardingStart =
  ( "onboarding" :> Capture "onboardingType" Kernel.Prelude.Text :> "start" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> Get '[JSON] Domain.Types.MerchantOnboarding.MerchantOnboardingAPI
  )

type MerchantOnboardingList =
  ( "onboarding" :> "list" :> QueryParam "requestorId" Kernel.Prelude.Text :> QueryParam "requestorRole" Domain.Types.MerchantOnboarding.RequestorRole
      :> Get
           '[JSON]
           [Domain.Types.MerchantOnboarding.MerchantOnboarding]
  )

type MerchantOnboardingStepSubmit =
  ( "onboarding" :> "step" :> Capture "stepId" Kernel.Prelude.Text :> "submit" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> ReqBody '[JSON] Data.Aeson.Value
      :> Post
           '[JSON]
           Domain.Types.MerchantOnboarding.MerchantOnboardingAPI
  )

type MerchantOnboardingStepUpdatePayload =
  ( "onboarding" :> "step" :> Capture "stepId" Kernel.Prelude.Text :> "updatePayload"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> QueryParam "requestorRole" Domain.Types.MerchantOnboarding.RequestorRole
      :> ReqBody '[JSON] Data.Aeson.Value
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type MerchantOnboardingStepReject =
  ( "onboarding" :> "step" :> Capture "stepId" Kernel.Prelude.Text :> "reject" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> ReqBody '[JSON] Data.Aeson.Value
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type MerchantOnboardingStepApprove =
  ( "onboarding" :> "step" :> Capture "stepId" Kernel.Prelude.Text :> "approve" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> ReqBody '[JSON] Data.Aeson.Value
      :> Post '[JSON] ApproveResponse
  )

type MerchantOnboardingStepUploadFile =
  ( "onboarding" :> "step" :> Capture "stepId" Kernel.Prelude.Text :> Capture "payloadKey" Kernel.Prelude.Text :> "uploadFile"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           UploadFileRequest
      :> Post
           '[JSON]
           UploadFileResponse
  )

type MerchantOnboardingReject =
  ( "onboarding" :> Capture "onboardingId" Kernel.Prelude.Text :> "reject" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> ReqBody '[JSON] Data.Aeson.Value
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type MerchantOnboadingListAll =
  ( "onboarding" :> "list" :> "all" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> QueryParam "status" Domain.Types.MerchantOnboarding.OnboardingStatus
      :> QueryParam
           "onboardingType"
           Domain.Types.MerchantOnboarding.OnboardingType
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           [Domain.Types.MerchantOnboarding.MerchantOnboardingAPI]
  )

type MerchantOnboardingStepList =
  ( "onboarding" :> Capture "onboardingId" Kernel.Prelude.Text :> "step" :> "list" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> Get '[JSON] [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep]
  )

type MerchantOnboardingGetFile =
  ( "onboarding" :> Capture "onboardingId" Kernel.Prelude.Text :> "file" :> Capture "fileId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> QueryParam "requestorRole" Domain.Types.MerchantOnboarding.RequestorRole
      :> Get
           '[JSON]
           Domain.Types.MerchantOnboarding.GetFileResponse
  )

type MerchantOnboardingCancel =
  ( "onboarding" :> Capture "onboardingId" Kernel.Prelude.Text :> "cancel" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorRole"
           Domain.Types.MerchantOnboarding.RequestorRole
      :> Get '[JSON] Kernel.Types.APISuccess.APISuccess
  )

data MerchantOnboardingAPIs = MerchantOnboardingAPIs
  { merchantOnboardingInfo :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> EulerHS.Types.EulerClient Domain.Types.MerchantOnboarding.MerchantOnboardingAPI,
    merchantOnboardingStart :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> EulerHS.Types.EulerClient Domain.Types.MerchantOnboarding.MerchantOnboardingAPI,
    merchantOnboardingList :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> EulerHS.Types.EulerClient [Domain.Types.MerchantOnboarding.MerchantOnboarding],
    merchantOnboardingStepSubmit :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> EulerHS.Types.EulerClient Domain.Types.MerchantOnboarding.MerchantOnboardingAPI,
    merchantOnboardingStepUpdatePayload :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    merchantOnboardingStepReject :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    merchantOnboardingStepApprove :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> EulerHS.Types.EulerClient ApproveResponse,
    merchantOnboardingStepUploadFile ::
      Kernel.Prelude.Text ->
        Kernel.Prelude.Text ->
        Kernel.Prelude.Maybe Kernel.Prelude.Text ->
        Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole ->
        ( Data.ByteString.Lazy.ByteString,
          UploadFileRequest
        ) ->
        EulerHS.Types.EulerClient UploadFileResponse,
    merchantOnboardingReject :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Data.Aeson.Value -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    merchantOnboadingListAll :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.OnboardingStatus -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient [Domain.Types.MerchantOnboarding.MerchantOnboardingAPI],
    merchantOnboardingStepList :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> EulerHS.Types.EulerClient [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep],
    merchantOnboardingGetFile :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> EulerHS.Types.EulerClient Domain.Types.MerchantOnboarding.GetFileResponse,
    merchantOnboardingCancel :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkMerchantOnboardingAPIs :: (Client EulerHS.Types.EulerClient API -> MerchantOnboardingAPIs)
mkMerchantOnboardingAPIs merchantOnboardingClient = (MerchantOnboardingAPIs {..})
  where
    merchantOnboardingInfo :<|> merchantOnboardingStart :<|> merchantOnboardingList :<|> merchantOnboardingStepSubmit :<|> merchantOnboardingStepUpdatePayload :<|> merchantOnboardingStepReject :<|> merchantOnboardingStepApprove :<|> merchantOnboardingStepUploadFile :<|> merchantOnboardingReject :<|> merchantOnboadingListAll :<|> merchantOnboardingStepList :<|> merchantOnboardingGetFile :<|> merchantOnboardingCancel = merchantOnboardingClient

data MerchantOnboardingUserActionType
  = MERCHANT_ONBOARDING_INFO
  | MERCHANT_ONBOARDING_START
  | MERCHANT_ONBOARDING_LIST
  | MERCHANT_ONBOARDING_STEP_SUBMIT
  | MERCHANT_ONBOARDING_STEP_UPDATE_PAYLOAD
  | MERCHANT_ONBOARDING_STEP_REJECT
  | MERCHANT_ONBOARDING_STEP_APPROVE
  | MERCHANT_ONBOARDING_STEP_UPLOAD_FILE
  | MERCHANT_ONBOARDING_REJECT
  | MERCHANT_ONBOADING_LIST_ALL
  | MERCHANT_ONBOARDING_STEP_LIST
  | MERCHANT_ONBOARDING_GET_FILE
  | MERCHANT_ONBOARDING_CANCEL
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''MerchantOnboardingUserActionType])
