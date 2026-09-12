{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.IssueManagement.Issue
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.IssueManagement.Issue
import qualified Data.Aeson
import qualified Domain.Action.Dashboard.IssueManagement.Issue
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified IssueManagement.Common
import qualified IssueManagement.Common.Dashboard.Issue
import qualified IssueManagement.Common.UI.Issue
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

type API = ("issue" :> (GetIssueCategoryList :<|> GetIssueList :<|> GetIssueInfo :<|> GetIssueInfoV2 :<|> PutIssueUpdateHelper :<|> PostIssueChatUpload :<|> PostIssueCommentHelper :<|> GetIssueMedia :<|> PostIssueTicketStatusCallBack :<|> PostIssueCategoryCreate :<|> PostIssueCategoryUpdate :<|> PostIssueOptionCreate :<|> PostIssueOptionUpdate :<|> PostIssueMessageUpsert :<|> GetIssueCategoryDetail :<|> GetIssueOptionDetail :<|> GetIssueMessageDetail :<|> GetIssueMessageList :<|> GetIssueOptionList :<|> DeleteIssueCategory :<|> DeleteIssueOption :<|> DeleteIssueMessage :<|> GetIssueCategoryFlowPreview :<|> GetIssueTranslations :<|> PostIssueBulkUpsertTranslations :<|> GetIssueConfig :<|> PostIssueConfigUpdate :<|> PostIssueCategoryReorder :<|> PostIssueOptionReorder :<|> PostIssueMessageReorder :<|> PostIssueCategoryCopy :<|> PostIssueCategoryDefaultCopy :<|> PostIssueCategoryAllCopy :<|> PostIssueChatMessageHelper :<|> GetIssueChatMessages :<|> PostIssueChatRead))

type GetIssueCategoryList = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CATEGORY_LIST" :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueCategoryList)

type GetIssueList = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_LIST" :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueList)

type GetIssueInfo = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_INFO" :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueInfo)

type GetIssueInfoV2 = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_INFO_V2" :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueInfoV2)

type PutIssueUpdateHelper = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/PUT_ISSUE_UPDATE" :> API.Types.ProviderPlatform.IssueManagement.Issue.PutIssueUpdateHelper)

type PostIssueChatUpload = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CHAT_UPLOAD" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueChatUpload)

type PostIssueCommentHelper = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_COMMENT" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueCommentHelper)

type GetIssueMedia = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_MEDIA" :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueMedia)

type PostIssueTicketStatusCallBack = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_TICKET_STATUS_CALL_BACK" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueTicketStatusCallBack)

type PostIssueCategoryCreate = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_CREATE" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueCategoryCreate)

type PostIssueCategoryUpdate = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_UPDATE" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueCategoryUpdate)

type PostIssueOptionCreate = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_CREATE" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueOptionCreate)

type PostIssueOptionUpdate = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_UPDATE" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueOptionUpdate)

type PostIssueMessageUpsert = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_MESSAGE_UPSERT" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueMessageUpsert)

type GetIssueCategoryDetail = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CATEGORY_DETAIL" :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueCategoryDetail)

type GetIssueOptionDetail = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_OPTION_DETAIL" :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueOptionDetail)

type GetIssueMessageDetail = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_MESSAGE_DETAIL" :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueMessageDetail)

type GetIssueMessageList = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_MESSAGE_LIST" :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueMessageList)

type GetIssueOptionList = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_OPTION_LIST" :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueOptionList)

type DeleteIssueCategory = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/DELETE_ISSUE_CATEGORY" :> API.Types.ProviderPlatform.IssueManagement.Issue.DeleteIssueCategory)

type DeleteIssueOption = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/DELETE_ISSUE_OPTION" :> API.Types.ProviderPlatform.IssueManagement.Issue.DeleteIssueOption)

type DeleteIssueMessage = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/DELETE_ISSUE_MESSAGE" :> API.Types.ProviderPlatform.IssueManagement.Issue.DeleteIssueMessage)

type GetIssueCategoryFlowPreview = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CATEGORY_FLOW_PREVIEW" :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueCategoryFlowPreview)

type GetIssueTranslations = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_TRANSLATIONS" :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueTranslations)

type PostIssueBulkUpsertTranslations = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_BULK_UPSERT_TRANSLATIONS" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueBulkUpsertTranslations)

type GetIssueConfig = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CONFIG" :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueConfig)

type PostIssueConfigUpdate = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CONFIG_UPDATE" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueConfigUpdate)

type PostIssueCategoryReorder = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_REORDER" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueCategoryReorder)

type PostIssueOptionReorder = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_REORDER" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueOptionReorder)

type PostIssueMessageReorder = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_MESSAGE_REORDER" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueMessageReorder)

type PostIssueCategoryCopy = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_COPY" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueCategoryCopy)

type PostIssueCategoryDefaultCopy = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_DEFAULT_COPY" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueCategoryDefaultCopy)

type PostIssueCategoryAllCopy = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_ALL_COPY" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueCategoryAllCopy)

type PostIssueChatMessageHelper = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CHAT_MESSAGE" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueChatMessageHelper)

type GetIssueChatMessages = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CHAT_MESSAGES" :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueChatMessages)

type PostIssueChatRead = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CHAT_READ" :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueChatRead)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getIssueCategoryList merchantId city :<|> getIssueList merchantId city :<|> getIssueInfo merchantId city :<|> getIssueInfoV2 merchantId city :<|> putIssueUpdate merchantId city :<|> postIssueChatUpload merchantId city :<|> postIssueComment merchantId city :<|> getIssueMedia merchantId city :<|> postIssueTicketStatusCallBack merchantId city :<|> postIssueCategoryCreate merchantId city :<|> postIssueCategoryUpdate merchantId city :<|> postIssueOptionCreate merchantId city :<|> postIssueOptionUpdate merchantId city :<|> postIssueMessageUpsert merchantId city :<|> getIssueCategoryDetail merchantId city :<|> getIssueOptionDetail merchantId city :<|> getIssueMessageDetail merchantId city :<|> getIssueMessageList merchantId city :<|> getIssueOptionList merchantId city :<|> deleteIssueCategory merchantId city :<|> deleteIssueOption merchantId city :<|> deleteIssueMessage merchantId city :<|> getIssueCategoryFlowPreview merchantId city :<|> getIssueTranslations merchantId city :<|> postIssueBulkUpsertTranslations merchantId city :<|> getIssueConfig merchantId city :<|> postIssueConfigUpdate merchantId city :<|> postIssueCategoryReorder merchantId city :<|> postIssueOptionReorder merchantId city :<|> postIssueMessageReorder merchantId city :<|> postIssueCategoryCopy merchantId city :<|> postIssueCategoryDefaultCopy merchantId city :<|> postIssueCategoryAllCopy merchantId city :<|> postIssueChatMessage merchantId city :<|> getIssueChatMessages merchantId city :<|> postIssueChatRead merchantId city

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
