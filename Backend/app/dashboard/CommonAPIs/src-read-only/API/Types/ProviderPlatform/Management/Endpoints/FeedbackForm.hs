{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.FeedbackForm where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified IssueManagement.Common
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data AnswerType
  = Text
  | Checkbox
  | Radio
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BadgeDetail = BadgeDetail
  { sendPN :: Kernel.Prelude.Bool,
    key :: Kernel.Prelude.Text,
    priority :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    contentWithTranslations :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Category
  = RIDE
  | DRIVER
  | VEHICLE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data CreateFeedbackFormReq = CreateFeedbackFormReq
  { categoryName :: Category,
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    question :: Kernel.Prelude.Text,
    questionTranslations :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    answer :: [Kernel.Prelude.Text],
    answerType :: AnswerType,
    badges :: Kernel.Prelude.Maybe [BadgeDetail],
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.MerchantOperatingCity),
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Merchant)
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateFeedbackFormReq where
  hideSecrets = Kernel.Prelude.identity

data CreateFeedbackFormRes = CreateFeedbackFormRes {feedbackFormId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FeedbackFormRes = FeedbackFormRes
  { id :: Kernel.Prelude.Text,
    categoryName :: Category,
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    question :: Kernel.Prelude.Text,
    questionTranslations :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    answer :: [Kernel.Prelude.Text],
    answerType :: AnswerType,
    badges :: Kernel.Prelude.Maybe [BadgeDetail],
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.MerchantOperatingCity),
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Merchant)
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateFeedbackFormReq = UpdateFeedbackFormReq
  { categoryName :: Kernel.Prelude.Maybe Category,
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    question :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    questionTranslations :: Kernel.Prelude.Maybe [IssueManagement.Common.Translation],
    answer :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    answerType :: Kernel.Prelude.Maybe AnswerType,
    badges :: Kernel.Prelude.Maybe [BadgeDetail],
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.MerchantOperatingCity),
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Merchant)
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateFeedbackFormReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("feedbackForm" :> (PostFeedbackFormCreate :<|> PutFeedbackFormUpdate :<|> DeleteFeedbackFormDelete :<|> GetFeedbackForm))

type PostFeedbackFormCreate = ("create" :> ReqBody '[JSON] CreateFeedbackFormReq :> Post '[JSON] CreateFeedbackFormRes)

type PutFeedbackFormUpdate = (Capture "feedbackFormId" Kernel.Prelude.Text :> "update" :> ReqBody '[JSON] UpdateFeedbackFormReq :> Put '[JSON] Kernel.Types.APISuccess.APISuccess)

type DeleteFeedbackFormDelete = (Capture "feedbackFormId" Kernel.Prelude.Text :> "delete" :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetFeedbackForm = (Capture "feedbackFormId" Kernel.Prelude.Text :> Get '[JSON] FeedbackFormRes)

data FeedbackFormAPIs = FeedbackFormAPIs
  { postFeedbackFormCreate :: CreateFeedbackFormReq -> EulerHS.Types.EulerClient CreateFeedbackFormRes,
    putFeedbackFormUpdate :: Kernel.Prelude.Text -> UpdateFeedbackFormReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteFeedbackFormDelete :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getFeedbackForm :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient FeedbackFormRes
  }

mkFeedbackFormAPIs :: (Client EulerHS.Types.EulerClient API -> FeedbackFormAPIs)
mkFeedbackFormAPIs feedbackFormClient = (FeedbackFormAPIs {..})
  where
    postFeedbackFormCreate :<|> putFeedbackFormUpdate :<|> deleteFeedbackFormDelete :<|> getFeedbackForm = feedbackFormClient

data FeedbackFormUserActionType
  = POST_FEEDBACK_FORM_CREATE
  | PUT_FEEDBACK_FORM_UPDATE
  | DELETE_FEEDBACK_FORM_DELETE
  | GET_FEEDBACK_FORM
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''FeedbackFormUserActionType])
