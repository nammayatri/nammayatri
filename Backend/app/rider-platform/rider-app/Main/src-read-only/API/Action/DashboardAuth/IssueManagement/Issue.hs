{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.IssueManagement.Issue
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.IssueManagement.Issue
import qualified Data.Aeson
import qualified Domain.Action.Dashboard.IssueManagement.Issue
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified IssueManagement.Common
import qualified IssueManagement.Common.Dashboard.Issue
import qualified IssueManagement.Common.UI.Issue
import qualified IssueManagement.Domain.Types.Issue.IssueApiIntegration
import qualified IssueManagement.Domain.Types.Issue.IssueCategory
import qualified IssueManagement.Domain.Types.Issue.IssueMessage
import qualified IssueManagement.Domain.Types.Issue.IssueOption
import qualified IssueManagement.Domain.Types.Issue.IssueReport
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("issueV2" :> (GetIssueCategoryList :<|> GetIssueList :<|> GetIssueInfo :<|> GetIssueInfoV2 :<|> PutIssueUpdateHelper :<|> PostIssueChatUpload :<|> PostIssueCommentHelper :<|> GetIssueMedia :<|> PostIssueTicketStatusCallBack :<|> PostIssueCategoryCreate :<|> PostIssueCategoryUpdate :<|> PostIssueOptionCreate :<|> PostIssueOptionUpdate :<|> PostIssueMessageUpsert :<|> PostIssueKaptureCreate :<|> GetIssueCategoryDetail :<|> GetIssueOptionDetail :<|> GetIssueMessageDetail :<|> GetIssueMessageList :<|> GetIssueOptionList :<|> DeleteIssueCategory :<|> DeleteIssueOption :<|> DeleteIssueMessage :<|> GetIssueCategoryFlowPreview :<|> GetIssueTranslations :<|> PostIssueBulkUpsertTranslations :<|> GetIssueConfig :<|> PostIssueConfigUpdate :<|> PostIssueCategoryReorder :<|> PostIssueOptionReorder :<|> PostIssueMessageReorder :<|> PostIssueCategoryCopy :<|> PostIssueCategoryDefaultCopy :<|> PostIssueCategoryAllCopy :<|> PostIssueChatMessageHelper :<|> GetIssueChatMessages :<|> PostIssueChatRead :<|> GetIssueApiIntegrationList :<|> PostIssueApiIntegrationUpsert :<|> PostIssueApiIntegrationDelete :<|> PostIssueApiIntegrationTest :<|> GetIssueFlowSimulate))

type GetIssueCategoryList = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CATEGORY_LIST" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueCategoryList)

type GetIssueList = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_LIST" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueList)

type GetIssueInfo = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_INFO" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueInfo)

type GetIssueInfoV2 = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_INFO_V2" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueInfoV2)

type PutIssueUpdateHelper = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/PUT_ISSUE_UPDATE" :> API.Types.RiderPlatform.IssueManagement.Issue.PutIssueUpdateHelper)

type PostIssueChatUpload = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CHAT_UPLOAD" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueChatUpload)

type PostIssueCommentHelper = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_COMMENT" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueCommentHelper)

type GetIssueMedia = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_MEDIA" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueMedia)

type PostIssueTicketStatusCallBack = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_TICKET_STATUS_CALL_BACK" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueTicketStatusCallBack)

type PostIssueCategoryCreate = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_CREATE" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueCategoryCreate)

type PostIssueCategoryUpdate = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_UPDATE" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueCategoryUpdate)

type PostIssueOptionCreate = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_CREATE" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueOptionCreate)

type PostIssueOptionUpdate = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_UPDATE" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueOptionUpdate)

type PostIssueMessageUpsert = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_MESSAGE_UPSERT" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueMessageUpsert)

type PostIssueKaptureCreate = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_KAPTURE_CREATE" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueKaptureCreate)

type GetIssueCategoryDetail = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CATEGORY_DETAIL" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueCategoryDetail)

type GetIssueOptionDetail = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_OPTION_DETAIL" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueOptionDetail)

type GetIssueMessageDetail = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_MESSAGE_DETAIL" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueMessageDetail)

type GetIssueMessageList = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_MESSAGE_LIST" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueMessageList)

type GetIssueOptionList = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_OPTION_LIST" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueOptionList)

type DeleteIssueCategory = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/DELETE_ISSUE_CATEGORY" :> API.Types.RiderPlatform.IssueManagement.Issue.DeleteIssueCategory)

type DeleteIssueOption = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/DELETE_ISSUE_OPTION" :> API.Types.RiderPlatform.IssueManagement.Issue.DeleteIssueOption)

type DeleteIssueMessage = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/DELETE_ISSUE_MESSAGE" :> API.Types.RiderPlatform.IssueManagement.Issue.DeleteIssueMessage)

type GetIssueCategoryFlowPreview = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CATEGORY_FLOW_PREVIEW" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueCategoryFlowPreview)

type GetIssueTranslations = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_TRANSLATIONS" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueTranslations)

type PostIssueBulkUpsertTranslations = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_BULK_UPSERT_TRANSLATIONS" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueBulkUpsertTranslations)

type GetIssueConfig = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CONFIG" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueConfig)

type PostIssueConfigUpdate = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CONFIG_UPDATE" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueConfigUpdate)

type PostIssueCategoryReorder = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_REORDER" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueCategoryReorder)

type PostIssueOptionReorder = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_REORDER" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueOptionReorder)

type PostIssueMessageReorder = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_MESSAGE_REORDER" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueMessageReorder)

type PostIssueCategoryCopy = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_COPY" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueCategoryCopy)

type PostIssueCategoryDefaultCopy = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_DEFAULT_COPY" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueCategoryDefaultCopy)

type PostIssueCategoryAllCopy = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_ALL_COPY" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueCategoryAllCopy)

type PostIssueChatMessageHelper = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CHAT_MESSAGE" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueChatMessageHelper)

type GetIssueChatMessages = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CHAT_MESSAGES" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueChatMessages)

type PostIssueChatRead = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CHAT_READ" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueChatRead)

type GetIssueApiIntegrationList = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_API_INTEGRATION_LIST" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueApiIntegrationList)

type PostIssueApiIntegrationUpsert = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_API_INTEGRATION_UPSERT" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueApiIntegrationUpsert)

type PostIssueApiIntegrationDelete = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_API_INTEGRATION_DELETE" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueApiIntegrationDelete)

type PostIssueApiIntegrationTest = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_API_INTEGRATION_TEST" :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueApiIntegrationTest)

type GetIssueFlowSimulate = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_FLOW_SIMULATE" :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueFlowSimulate)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getIssueCategoryList merchantId city :<|> getIssueList merchantId city :<|> getIssueInfo merchantId city :<|> getIssueInfoV2 merchantId city :<|> putIssueUpdate merchantId city :<|> postIssueChatUpload merchantId city :<|> postIssueComment merchantId city :<|> getIssueMedia merchantId city :<|> postIssueTicketStatusCallBack merchantId city :<|> postIssueCategoryCreate merchantId city :<|> postIssueCategoryUpdate merchantId city :<|> postIssueOptionCreate merchantId city :<|> postIssueOptionUpdate merchantId city :<|> postIssueMessageUpsert merchantId city :<|> postIssueKaptureCreate merchantId city :<|> getIssueCategoryDetail merchantId city :<|> getIssueOptionDetail merchantId city :<|> getIssueMessageDetail merchantId city :<|> getIssueMessageList merchantId city :<|> getIssueOptionList merchantId city :<|> deleteIssueCategory merchantId city :<|> deleteIssueOption merchantId city :<|> deleteIssueMessage merchantId city :<|> getIssueCategoryFlowPreview merchantId city :<|> getIssueTranslations merchantId city :<|> postIssueBulkUpsertTranslations merchantId city :<|> getIssueConfig merchantId city :<|> postIssueConfigUpdate merchantId city :<|> postIssueCategoryReorder merchantId city :<|> postIssueOptionReorder merchantId city :<|> postIssueMessageReorder merchantId city :<|> postIssueCategoryCopy merchantId city :<|> postIssueCategoryDefaultCopy merchantId city :<|> postIssueCategoryAllCopy merchantId city :<|> postIssueChatMessage merchantId city :<|> getIssueChatMessages merchantId city :<|> postIssueChatRead merchantId city :<|> getIssueApiIntegrationList merchantId city :<|> postIssueApiIntegrationUpsert merchantId city :<|> postIssueApiIntegrationDelete merchantId city :<|> postIssueApiIntegrationTest merchantId city :<|> getIssueFlowSimulate merchantId city

getIssueCategoryList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueCategoryListRes)
getIssueCategoryList a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueCategoryList a3 a2

getIssueList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe IssueManagement.Common.IssueStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueReportListResponse)
getIssueList a15 a14 _a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueList a15 a14 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getIssueInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueInfoDRes)
getIssueInfo a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueInfo a4 a3 a1

getIssueInfoV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueInfoDRes)
getIssueInfoV2 a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueInfoV2 a5 a4 a2 a1

putIssueUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.IssueUpdateByUserReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIssueUpdate a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.putIssueUpdate a5 a4 a2 a1

postIssueChatUpload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> IssueManagement.Common.UI.Issue.IssueMediaUploadReq -> Environment.FlowHandler IssueManagement.Common.UI.Issue.IssueMediaUploadRes)
postIssueChatUpload a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueChatUpload a4 a3 a1

postIssueComment :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.IssueAddCommentByUserReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueComment a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueComment a5 a4 a2 a1

getIssueMedia :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Prelude.Text)
getIssueMedia a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueMedia a4 a3 a1

postIssueTicketStatusCallBack :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueTicketStatusCallBack a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueTicketStatusCallBack a4 a3 a1

postIssueCategoryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryRes)
postIssueCategoryCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueCategoryCreate a4 a3 a1

postIssueCategoryUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> IssueManagement.Common.Dashboard.Issue.UpdateIssueCategoryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueCategoryUpdate a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueCategoryUpdate a5 a4 a2 a1

postIssueOptionCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage -> IssueManagement.Common.Dashboard.Issue.CreateIssueOptionReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.CreateIssueOptionRes)
postIssueOptionCreate a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueOptionCreate a6 a5 a3 a2 a1

postIssueOptionUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption -> IssueManagement.Common.Dashboard.Issue.UpdateIssueOptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueOptionUpdate a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueOptionUpdate a5 a4 a2 a1

postIssueMessageUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageRes)
postIssueMessageUpsert a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueMessageUpsert a4 a3 a1

postIssueKaptureCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> IssueManagement.Common.Dashboard.Issue.IssueReportReqV2 -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueKaptureCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueKaptureCreate a4 a3 a1

getIssueCategoryDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueCategoryDetailRes)
getIssueCategoryDetail a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueCategoryDetail a5 a4 a2 a1

getIssueOptionDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueOptionDetailRes)
getIssueOptionDetail a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueOptionDetail a5 a4 a2 a1

getIssueMessageDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueMessageDetailRes)
getIssueMessageDetail a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueMessageDetail a5 a4 a2 a1

getIssueMessageList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueMessageListRes)
getIssueMessageList a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueMessageList a7 a6 a4 a3 a2 a1

getIssueOptionList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueOptionListDRes)
getIssueOptionList a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueOptionList a7 a6 a4 a3 a2 a1

deleteIssueCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIssueCategory a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.deleteIssueCategory a4 a3 a1

deleteIssueOption :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIssueOption a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.deleteIssueOption a4 a3 a1

deleteIssueMessage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIssueMessage a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.deleteIssueMessage a4 a3 a1

getIssueCategoryFlowPreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueCategoryFlowPreviewRes)
getIssueCategoryFlowPreview a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueCategoryFlowPreview a5 a4 a2 a1

getIssueTranslations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueTranslationListRes)
getIssueTranslations a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueTranslations a4 a3 a1

postIssueBulkUpsertTranslations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> IssueManagement.Common.Dashboard.Issue.BulkUpsertTranslationsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueBulkUpsertTranslations a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueBulkUpsertTranslations a4 a3 a1

getIssueConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueConfigRes)
getIssueConfig a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueConfig a3 a2

postIssueConfigUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> IssueManagement.Common.Dashboard.Issue.UpdateIssueConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueConfigUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueConfigUpdate a4 a3 a1

postIssueCategoryReorder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> IssueManagement.Common.Dashboard.Issue.ReorderIssueCategoryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueCategoryReorder a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueCategoryReorder a4 a3 a1

postIssueOptionReorder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> IssueManagement.Common.Dashboard.Issue.ReorderIssueOptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueOptionReorder a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueOptionReorder a4 a3 a1

postIssueMessageReorder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> IssueManagement.Common.Dashboard.Issue.ReorderIssueMessageReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueMessageReorder a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueMessageReorder a4 a3 a1

postIssueCategoryCopy :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> IssueManagement.Common.Dashboard.Issue.CopyIssueCategoryReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.CopyIssueCategoryRes)
postIssueCategoryCopy a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueCategoryCopy a4 a3 a1

postIssueCategoryDefaultCopy :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.CopyAllIssueCategoryRes)
postIssueCategoryDefaultCopy a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueCategoryDefaultCopy a3 a2

postIssueCategoryAllCopy :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> IssueManagement.Common.Dashboard.Issue.CopyAllIssueCategoryReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.CopyAllIssueCategoryRes)
postIssueCategoryAllCopy a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueCategoryAllCopy a4 a3 a1

postIssueChatMessage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.SendChatMessageByUserReq -> Environment.FlowHandler IssueManagement.Common.UI.Issue.ChatMessageItem)
postIssueChatMessage a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueChatMessage a5 a4 a2 a1

getIssueChatMessages :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler [IssueManagement.Common.UI.Issue.ChatMessageItem])
getIssueChatMessages a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueChatMessages a6 a5 a3 a2 a1

postIssueChatRead :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.MarkChatReadByUserReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueChatRead a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueChatRead a5 a4 a2 a1

getIssueApiIntegrationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.ApiIntegrationListRes)
getIssueApiIntegrationList a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueApiIntegrationList a3 a2

postIssueApiIntegrationUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> IssueManagement.Common.Dashboard.Issue.UpsertApiIntegrationReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.UpsertApiIntegrationRes)
postIssueApiIntegrationUpsert a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueApiIntegrationUpsert a4 a3 a1

postIssueApiIntegrationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueApiIntegration.IssueApiIntegration -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueApiIntegrationDelete a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueApiIntegrationDelete a4 a3 a1

postIssueApiIntegrationTest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> IssueManagement.Common.Dashboard.Issue.TestApiIntegrationReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.TestApiIntegrationRes)
postIssueApiIntegrationTest a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueApiIntegrationTest a4 a3 a1

getIssueFlowSimulate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Environment.FlowHandler IssueManagement.Common.UI.Issue.IssueOptionListRes)
getIssueFlowSimulate a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueFlowSimulate a7 a6 a4 a3 a2 a1
