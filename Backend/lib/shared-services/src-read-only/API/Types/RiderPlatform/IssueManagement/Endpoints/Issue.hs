{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.IssueManagement.Endpoints.Issue where

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
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant hiding (Summary)
import Servant.Client

type API = ("issueV2" :> (GetIssueCategoryList :<|> GetIssueList :<|> GetIssueInfo :<|> GetIssueInfoV2 :<|> PutIssueUpdateHelper :<|> PostIssueCommentHelper :<|> GetIssueMedia :<|> PostIssueTicketStatusCallBack :<|> PostIssueCategoryCreate :<|> PostIssueCategoryUpdate :<|> PostIssueOptionCreate :<|> PostIssueOptionUpdate :<|> PostIssueMessageUpsert :<|> PostIssueKaptureCreate :<|> GetIssueCategoryDetail :<|> GetIssueOptionDetail :<|> GetIssueMessageDetail :<|> GetIssueMessageList :<|> GetIssueOptionList :<|> DeleteIssueCategory :<|> DeleteIssueOption :<|> DeleteIssueMessage :<|> GetIssueCategoryFlowPreview :<|> GetIssueTranslations :<|> PostIssueBulkUpsertTranslations :<|> GetIssueConfig :<|> PostIssueConfigUpdate :<|> PostIssueCategoryReorder :<|> PostIssueOptionReorder :<|> PostIssueMessageReorder))

type GetIssueCategoryList = ("category" :> Get '[JSON] IssueManagement.Common.Dashboard.Issue.IssueCategoryListRes)

type GetIssueList =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "status" IssueManagement.Common.IssueStatus
      :> QueryParam
           "category"
           (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory)
      :> QueryParam
           "categoryName"
           Kernel.Prelude.Text
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
      :> QueryParam
           "descriptionSearch"
           Kernel.Prelude.Text
      :> QueryParam
           "fromDate"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "toDate"
           Kernel.Prelude.UTCTime
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

type PostIssueKaptureCreate = ("kapture" :> "create" :> ReqBody '[JSON] IssueManagement.Common.Dashboard.Issue.IssueReportReqV2 :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetIssueCategoryDetail =
  ( "category" :> Capture "categoryId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) :> "detail"
      :> QueryParam
           "language"
           Kernel.External.Types.Language
      :> Get '[JSON] IssueManagement.Common.Dashboard.Issue.IssueCategoryDetailRes
  )

type GetIssueOptionDetail =
  ( "option" :> Capture "optionId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption) :> "detail"
      :> QueryParam
           "language"
           Kernel.External.Types.Language
      :> Get '[JSON] IssueManagement.Common.Dashboard.Issue.IssueOptionDetailRes
  )

type GetIssueMessageDetail =
  ( "message" :> Capture "messageId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage) :> "detail"
      :> QueryParam
           "language"
           Kernel.External.Types.Language
      :> Get '[JSON] IssueManagement.Common.Dashboard.Issue.IssueMessageDetailRes
  )

type GetIssueMessageList =
  ( "message" :> "list" :> QueryParam "categoryId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory)
      :> QueryParam
           "optionId"
           (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption)
      :> QueryParam
           "isActive"
           Kernel.Prelude.Bool
      :> QueryParam
           "language"
           Kernel.External.Types.Language
      :> Get
           '[JSON]
           IssueManagement.Common.Dashboard.Issue.IssueMessageListRes
  )

type GetIssueOptionList =
  ( "option" :> "list" :> QueryParam "categoryId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory)
      :> QueryParam
           "messageId"
           (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage)
      :> QueryParam
           "isActive"
           Kernel.Prelude.Bool
      :> QueryParam
           "language"
           Kernel.External.Types.Language
      :> Get
           '[JSON]
           IssueManagement.Common.Dashboard.Issue.IssueOptionListRes
  )

type DeleteIssueCategory =
  ( "category" :> Capture "categoryId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) :> "delete"
      :> Delete
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type DeleteIssueOption =
  ( "option" :> Capture "optionId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption) :> "delete"
      :> Delete
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type DeleteIssueMessage =
  ( "message" :> Capture "messageId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage) :> "delete"
      :> Delete
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetIssueCategoryFlowPreview =
  ( "category" :> Capture "categoryId" (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) :> "preview"
      :> QueryParam
           "language"
           Kernel.External.Types.Language
      :> Get '[JSON] IssueManagement.Common.Dashboard.Issue.IssueCategoryFlowPreviewRes
  )

type GetIssueTranslations = ("translation" :> "list" :> MandatoryQueryParam "sentence" Kernel.Prelude.Text :> Get '[JSON] IssueManagement.Common.Dashboard.Issue.IssueTranslationListRes)

type PostIssueBulkUpsertTranslations =
  ( "translation" :> "bulk" :> ReqBody '[JSON] IssueManagement.Common.Dashboard.Issue.BulkUpsertTranslationsReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetIssueConfig = ("config" :> Get '[JSON] IssueManagement.Common.Dashboard.Issue.IssueConfigRes)

type PostIssueConfigUpdate = ("config" :> "update" :> ReqBody '[JSON] IssueManagement.Common.Dashboard.Issue.UpdateIssueConfigReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostIssueCategoryReorder = ("category" :> "reorder" :> ReqBody '[JSON] IssueManagement.Common.Dashboard.Issue.ReorderIssueCategoryReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostIssueOptionReorder = ("option" :> "reorder" :> ReqBody '[JSON] IssueManagement.Common.Dashboard.Issue.ReorderIssueOptionReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostIssueMessageReorder = ("message" :> "reorder" :> ReqBody '[JSON] IssueManagement.Common.Dashboard.Issue.ReorderIssueMessageReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data IssueAPIs = IssueAPIs
  { getIssueCategoryList :: EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.IssueCategoryListRes,
    getIssueList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe IssueManagement.Common.IssueStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.IssueReportListResponse,
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
      EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageRes,
    postIssueKaptureCreate :: IssueManagement.Common.Dashboard.Issue.IssueReportReqV2 -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getIssueCategoryDetail :: Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.IssueCategoryDetailRes,
    getIssueOptionDetail :: Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.IssueOptionDetailRes,
    getIssueMessageDetail :: Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.IssueMessageDetailRes,
    getIssueMessageList :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.IssueMessageListRes,
    getIssueOptionList :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.IssueOptionListRes,
    deleteIssueCategory :: Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteIssueOption :: Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteIssueMessage :: Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getIssueCategoryFlowPreview :: Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.IssueCategoryFlowPreviewRes,
    getIssueTranslations :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.IssueTranslationListRes,
    postIssueBulkUpsertTranslations :: IssueManagement.Common.Dashboard.Issue.BulkUpsertTranslationsReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getIssueConfig :: EulerHS.Types.EulerClient IssueManagement.Common.Dashboard.Issue.IssueConfigRes,
    postIssueConfigUpdate :: IssueManagement.Common.Dashboard.Issue.UpdateIssueConfigReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postIssueCategoryReorder :: IssueManagement.Common.Dashboard.Issue.ReorderIssueCategoryReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postIssueOptionReorder :: IssueManagement.Common.Dashboard.Issue.ReorderIssueOptionReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postIssueMessageReorder :: IssueManagement.Common.Dashboard.Issue.ReorderIssueMessageReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkIssueAPIs :: (Client EulerHS.Types.EulerClient API -> IssueAPIs)
mkIssueAPIs issueClient = (IssueAPIs {..})
  where
    getIssueCategoryList :<|> getIssueList :<|> getIssueInfo :<|> getIssueInfoV2 :<|> putIssueUpdate :<|> postIssueComment :<|> getIssueMedia :<|> postIssueTicketStatusCallBack :<|> postIssueCategoryCreate :<|> postIssueCategoryUpdate :<|> postIssueOptionCreate :<|> postIssueOptionUpdate :<|> postIssueMessageUpsert :<|> postIssueKaptureCreate :<|> getIssueCategoryDetail :<|> getIssueOptionDetail :<|> getIssueMessageDetail :<|> getIssueMessageList :<|> getIssueOptionList :<|> deleteIssueCategory :<|> deleteIssueOption :<|> deleteIssueMessage :<|> getIssueCategoryFlowPreview :<|> getIssueTranslations :<|> postIssueBulkUpsertTranslations :<|> getIssueConfig :<|> postIssueConfigUpdate :<|> postIssueCategoryReorder :<|> postIssueOptionReorder :<|> postIssueMessageReorder = issueClient

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
  | POST_ISSUE_KAPTURE_CREATE
  | GET_ISSUE_CATEGORY_DETAIL
  | GET_ISSUE_OPTION_DETAIL
  | GET_ISSUE_MESSAGE_DETAIL
  | GET_ISSUE_MESSAGE_LIST
  | GET_ISSUE_OPTION_LIST
  | DELETE_ISSUE_CATEGORY
  | DELETE_ISSUE_OPTION
  | DELETE_ISSUE_MESSAGE
  | GET_ISSUE_CATEGORY_FLOW_PREVIEW
  | GET_ISSUE_TRANSLATIONS
  | POST_ISSUE_BULK_UPSERT_TRANSLATIONS
  | GET_ISSUE_CONFIG
  | POST_ISSUE_CONFIG_UPDATE
  | POST_ISSUE_CATEGORY_REORDER
  | POST_ISSUE_OPTION_REORDER
  | POST_ISSUE_MESSAGE_REORDER
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''IssueUserActionType])
