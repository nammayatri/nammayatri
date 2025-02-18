{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.IssueManagement.Endpoints.Issue where

import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified IssueManagement.Common
import qualified IssueManagement.Common.Dashboard.Issue
import qualified IssueManagement.Domain.Types.Issue.IssueCategory
import qualified IssueManagement.Domain.Types.Issue.IssueMessage
import qualified IssueManagement.Domain.Types.Issue.IssueOption
import qualified IssueManagement.Domain.Types.Issue.IssueReport
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant hiding (Summary)
import Servant.Client

type API = ("issue" :> (GetIssueCategoryList :<|> GetIssueList :<|> GetIssueInfo :<|> GetIssueInfoV2 :<|> PutIssueUpdateHelper :<|> PostIssueCommentHelper :<|> GetIssueMedia :<|> PostIssueTicketStatusCallBack :<|> PostIssueCategoryCreate :<|> PostIssueCategoryUpdate :<|> PostIssueOptionCreate :<|> PostIssueOptionUpdate :<|> PostIssueMessageUpsert))

type GetIssueCategoryList = ("category" :> Get '[JSON] IssueManagement.Common.Dashboard.Issue.IssueCategoryListRes)

type GetIssueList =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "status" IssueManagement.Common.IssueStatus
      :> QueryParam
           "category"
           (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory)
      :> QueryParam
           "assignee"
           Kernel.Prelude.Text
      :> QueryParam
           "countryCode"
           Kernel.Prelude.Text
      :> QueryParam
           "phoneNumber"
           Kernel.Prelude.Text
      :> QueryParam
           "rideShortId"
           (Kernel.Types.Id.ShortId IssueManagement.Common.Ride)
      :> Get
           '[JSON]
           IssueManagement.Common.Dashboard.Issue.IssueReportListResponse
  )

type GetIssueInfo = (Capture "issueId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) :> "info" :> Get '[JSON] IssueManagement.Common.Dashboard.Issue.IssueInfoRes)

type GetIssueInfoV2 =
  ( "info" :> QueryParam "issueId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport)
      :> QueryParam
           "issueShortId"
           (Kernel.Types.Id.ShortId IssueManagement.Domain.Types.Issue.IssueReport.IssueReport)
      :> Get
           '[JSON]
           IssueManagement.Common.Dashboard.Issue.IssueInfoRes
  )

type PutIssueUpdate =
  ( Capture "issueId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) :> "update"
      :> ReqBody
           '[JSON]
           IssueManagement.Common.Dashboard.Issue.IssueUpdateReq
      :> Put '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PutIssueUpdateHelper =
  ( Capture "issueId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) :> "update"
      :> ReqBody
           '[JSON]
           IssueManagement.Common.Dashboard.Issue.IssueUpdateByUserReq
      :> Put '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostIssueComment =
  ( Capture "issueId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) :> "comment"
      :> ReqBody
           '[JSON]
           IssueManagement.Common.Dashboard.Issue.IssueAddCommentReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostIssueCommentHelper =
  ( Capture "issueId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) :> "comment"
      :> ReqBody
           '[JSON]
           IssueManagement.Common.Dashboard.Issue.IssueAddCommentByUserReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetIssueMedia = ("media" :> MandatoryQueryParam "filePath" Kernel.Prelude.Text :> Get '[JSON] Kernel.Prelude.Text)

type PostIssueTicketStatusCallBack = ("kapture" :> "ticketStatus" :> ReqBody '[JSON] Data.Aeson.Value :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostIssueCategoryCreate =
  ( "category" :> "create" :> ReqBody '[JSON] IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryReq
      :> Post
           '[JSON]
           IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryRes
  )

type PostIssueCategoryUpdate =
  ( "category" :> "update" :> MandatoryQueryParam "issueCategoryId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory)
      :> ReqBody
           '[JSON]
           IssueManagement.Common.Dashboard.Issue.UpdateIssueCategoryReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostIssueOptionCreate =
  ( "option" :> "create"
      :> MandatoryQueryParam
           "issueCategoryId"
           (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory)
      :> MandatoryQueryParam
           "issueMessageId"
           (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage)
      :> ReqBody
           '[JSON]
           IssueManagement.Common.Dashboard.Issue.CreateIssueOptionReq
      :> Post
           '[JSON]
           IssueManagement.Common.Dashboard.Issue.CreateIssueOptionRes
  )

type PostIssueOptionUpdate =
  ( "option" :> "update" :> MandatoryQueryParam "issueOptionid" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption)
      :> ReqBody
           '[JSON]
           IssueManagement.Common.Dashboard.Issue.UpdateIssueOptionReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostIssueMessageUpsert =
  ( "message" :> "upsert" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageReq
      :> Post
           '[JSON]
           IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageRes
  )

data IssueAPIs = IssueAPIs
  { getIssueCategoryList :: EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.IssueCategoryListRes,
    getIssueList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe IssueManagement.Common.IssueStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Common.Ride) -> EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.IssueReportListResponse,
    getIssueInfo :: Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.IssueInfoRes,
    getIssueInfoV2 :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) -> EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.IssueInfoRes,
    putIssueUpdate :: Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.IssueUpdateByUserReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postIssueComment :: Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.IssueAddCommentByUserReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getIssueMedia :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Prelude.Text,
    postIssueTicketStatusCallBack :: Data.Aeson.Value -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postIssueCategoryCreate :: IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryReq -> EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryRes,
    postIssueCategoryUpdate :: Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> IssueManagement.Common.Dashboard.Issue.UpdateIssueCategoryReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postIssueOptionCreate :: Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage -> IssueManagement.Common.Dashboard.Issue.CreateIssueOptionReq -> EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.CreateIssueOptionRes,
    postIssueOptionUpdate :: Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption -> IssueManagement.Common.Dashboard.Issue.UpdateIssueOptionReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postIssueMessageUpsert ::
      ( Data.ByteString.Lazy.ByteString,
        IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageReq
      ) ->
      EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageRes
  }

mkIssueAPIs :: (Client EulerHS.Types.EulerClient API -> IssueAPIs)
mkIssueAPIs issueClient = (IssueAPIs {..})
  where
    getIssueCategoryList :<|> getIssueList :<|> getIssueInfo :<|> getIssueInfoV2 :<|> putIssueUpdate :<|> postIssueComment :<|> getIssueMedia :<|> postIssueTicketStatusCallBack :<|> postIssueCategoryCreate :<|> postIssueCategoryUpdate :<|> postIssueOptionCreate :<|> postIssueOptionUpdate :<|> postIssueMessageUpsert = issueClient

data IssueUserActionType
  = GET_ISSUE_CATEGORY_LIST
  | GET_ISSUE_LIST
  | GET_ISSUE_INFO
  | GET_ISSUE_INFO_V2
  | PUT_ISSUE_UPDATE
  | POST_ISSUE_COMMENT
  | GET_ISSUE_MEDIA
  | POST_ISSUE_TICKET_STATUS_CALL_BACK
  | POST_ISSUE_CATEGORY_CREATE
  | POST_ISSUE_CATEGORY_UPDATE
  | POST_ISSUE_OPTION_CREATE
  | POST_ISSUE_OPTION_UPDATE
  | POST_ISSUE_MESSAGE_UPSERT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''IssueUserActionType])
