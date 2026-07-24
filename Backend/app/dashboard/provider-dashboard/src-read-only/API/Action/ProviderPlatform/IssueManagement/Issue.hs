{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.IssueManagement.Issue
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.IssueManagement
import qualified API.Types.ProviderPlatform.IssueManagement.Issue
import qualified Data.Aeson
import qualified Domain.Action.ProviderPlatform.IssueManagement.Issue
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
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
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("issue" :> (GetIssueCategoryList :<|> GetIssueList :<|> GetIssueInfo :<|> GetIssueInfoV2 :<|> PutIssueUpdate :<|> PostIssueChatUpload :<|> PostIssueComment :<|> GetIssueMedia :<|> PostIssueTicketStatusCallBack :<|> PostIssueCategoryCreate :<|> PostIssueCategoryUpdate :<|> PostIssueOptionCreate :<|> PostIssueOptionUpdate :<|> PostIssueMessageUpsert :<|> GetIssueCategoryDetail :<|> GetIssueOptionDetail :<|> GetIssueMessageDetail :<|> GetIssueMessageList :<|> GetIssueOptionList :<|> DeleteIssueCategory :<|> DeleteIssueOption :<|> DeleteIssueMessage :<|> GetIssueCategoryFlowPreview :<|> GetIssueTranslations :<|> PostIssueBulkUpsertTranslations :<|> GetIssueConfig :<|> PostIssueConfigUpdate :<|> PostIssueCategoryReorder :<|> PostIssueOptionReorder :<|> PostIssueMessageReorder :<|> PostIssueCategoryCopy :<|> PostIssueCategoryDefaultCopy :<|> PostIssueCategoryAllCopy :<|> PostIssueChatMessage :<|> GetIssueChatMessages :<|> PostIssueChatRead))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getIssueCategoryList merchantId city :<|> getIssueList merchantId city :<|> getIssueInfo merchantId city :<|> getIssueInfoV2 merchantId city :<|> putIssueUpdate merchantId city :<|> postIssueChatUpload merchantId city :<|> postIssueComment merchantId city :<|> getIssueMedia merchantId city :<|> postIssueTicketStatusCallBack merchantId city :<|> postIssueCategoryCreate merchantId city :<|> postIssueCategoryUpdate merchantId city :<|> postIssueOptionCreate merchantId city :<|> postIssueOptionUpdate merchantId city :<|> postIssueMessageUpsert merchantId city :<|> getIssueCategoryDetail merchantId city :<|> getIssueOptionDetail merchantId city :<|> getIssueMessageDetail merchantId city :<|> getIssueMessageList merchantId city :<|> getIssueOptionList merchantId city :<|> deleteIssueCategory merchantId city :<|> deleteIssueOption merchantId city :<|> deleteIssueMessage merchantId city :<|> getIssueCategoryFlowPreview merchantId city :<|> getIssueTranslations merchantId city :<|> postIssueBulkUpsertTranslations merchantId city :<|> getIssueConfig merchantId city :<|> postIssueConfigUpdate merchantId city :<|> postIssueCategoryReorder merchantId city :<|> postIssueOptionReorder merchantId city :<|> postIssueMessageReorder merchantId city :<|> postIssueCategoryCopy merchantId city :<|> postIssueCategoryDefaultCopy merchantId city :<|> postIssueCategoryAllCopy merchantId city :<|> postIssueChatMessage merchantId city :<|> getIssueChatMessages merchantId city :<|> postIssueChatRead merchantId city

type GetIssueCategoryList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.GET_ISSUE_CATEGORY_LIST)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueCategoryList
  )

type GetIssueList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.GET_ISSUE_LIST)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueList
  )

type GetIssueInfo =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.GET_ISSUE_INFO)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueInfo
  )

type GetIssueInfoV2 =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.GET_ISSUE_INFO_V2)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueInfoV2
  )

type PutIssueUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.PUT_ISSUE_UPDATE)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PutIssueUpdate
  )

type PostIssueChatUpload =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_CHAT_UPLOAD)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueChatUpload
  )

type PostIssueComment =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_COMMENT)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueComment
  )

type GetIssueMedia =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.GET_ISSUE_MEDIA)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueMedia
  )

type PostIssueTicketStatusCallBack =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_TICKET_STATUS_CALL_BACK)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueTicketStatusCallBack
  )

type PostIssueCategoryCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_CATEGORY_CREATE)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueCategoryCreate
  )

type PostIssueCategoryUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_CATEGORY_UPDATE)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueCategoryUpdate
  )

type PostIssueOptionCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_OPTION_CREATE)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueOptionCreate
  )

type PostIssueOptionUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_OPTION_UPDATE)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueOptionUpdate
  )

type PostIssueMessageUpsert =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_MESSAGE_UPSERT)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueMessageUpsert
  )

type GetIssueCategoryDetail =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.GET_ISSUE_CATEGORY_DETAIL)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueCategoryDetail
  )

type GetIssueOptionDetail =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.GET_ISSUE_OPTION_DETAIL)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueOptionDetail
  )

type GetIssueMessageDetail =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.GET_ISSUE_MESSAGE_DETAIL)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueMessageDetail
  )

type GetIssueMessageList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.GET_ISSUE_MESSAGE_LIST)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueMessageList
  )

type GetIssueOptionList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.GET_ISSUE_OPTION_LIST)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueOptionList
  )

type DeleteIssueCategory =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.DELETE_ISSUE_CATEGORY)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.DeleteIssueCategory
  )

type DeleteIssueOption =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.DELETE_ISSUE_OPTION)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.DeleteIssueOption
  )

type DeleteIssueMessage =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.DELETE_ISSUE_MESSAGE)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.DeleteIssueMessage
  )

type GetIssueCategoryFlowPreview =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.GET_ISSUE_CATEGORY_FLOW_PREVIEW)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueCategoryFlowPreview
  )

type GetIssueTranslations =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.GET_ISSUE_TRANSLATIONS)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueTranslations
  )

type PostIssueBulkUpsertTranslations =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_BULK_UPSERT_TRANSLATIONS)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueBulkUpsertTranslations
  )

type GetIssueConfig =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.GET_ISSUE_CONFIG)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueConfig
  )

type PostIssueConfigUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_CONFIG_UPDATE)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueConfigUpdate
  )

type PostIssueCategoryReorder =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_CATEGORY_REORDER)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueCategoryReorder
  )

type PostIssueOptionReorder =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_OPTION_REORDER)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueOptionReorder
  )

type PostIssueMessageReorder =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_MESSAGE_REORDER)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueMessageReorder
  )

type PostIssueCategoryCopy =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_CATEGORY_COPY)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueCategoryCopy
  )

type PostIssueCategoryDefaultCopy =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_CATEGORY_DEFAULT_COPY)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueCategoryDefaultCopy
  )

type PostIssueCategoryAllCopy =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_CATEGORY_ALL_COPY)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueCategoryAllCopy
  )

type PostIssueChatMessage =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_CHAT_MESSAGE)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueChatMessage
  )

type GetIssueChatMessages =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.GET_ISSUE_CHAT_MESSAGES)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.GetIssueChatMessages
  )

type PostIssueChatRead =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_ISSUE_MANAGEMENT / 'API.Types.ProviderPlatform.IssueManagement.ISSUE / 'API.Types.ProviderPlatform.IssueManagement.Issue.POST_ISSUE_CHAT_READ)
      :> API.Types.ProviderPlatform.IssueManagement.Issue.PostIssueChatRead
  )

getIssueCategoryList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueCategoryListRes)
getIssueCategoryList merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.getIssueCategoryList merchantShortId opCity apiTokenInfo

getIssueList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe IssueManagement.Common.IssueStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueReportListResponse)
getIssueList merchantShortId opCity apiTokenInfo limit offset status category categoryName assignee countryCode phoneNumber rideShortId descriptionSearch fromDate toDate = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.getIssueList merchantShortId opCity apiTokenInfo limit offset status category categoryName assignee countryCode phoneNumber rideShortId descriptionSearch fromDate toDate

getIssueInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueInfoDRes)
getIssueInfo merchantShortId opCity apiTokenInfo issueId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.getIssueInfo merchantShortId opCity apiTokenInfo issueId

getIssueInfoV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueInfoDRes)
getIssueInfoV2 merchantShortId opCity apiTokenInfo issueId issueShortId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.getIssueInfoV2 merchantShortId opCity apiTokenInfo issueId issueShortId

putIssueUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.IssueUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIssueUpdate merchantShortId opCity apiTokenInfo issueId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.putIssueUpdate merchantShortId opCity apiTokenInfo issueId req

postIssueChatUpload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.UI.Issue.IssueMediaUploadReq -> Environment.FlowHandler IssueManagement.Common.UI.Issue.IssueMediaUploadRes)
postIssueChatUpload merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueChatUpload merchantShortId opCity apiTokenInfo req

postIssueComment :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.IssueAddCommentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueComment merchantShortId opCity apiTokenInfo issueId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueComment merchantShortId opCity apiTokenInfo issueId req

getIssueMedia :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Prelude.Text)
getIssueMedia merchantShortId opCity apiTokenInfo filePath = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.getIssueMedia merchantShortId opCity apiTokenInfo filePath

postIssueTicketStatusCallBack :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueTicketStatusCallBack merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueTicketStatusCallBack merchantShortId opCity apiTokenInfo req

postIssueCategoryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryRes)
postIssueCategoryCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueCategoryCreate merchantShortId opCity apiTokenInfo req

postIssueCategoryUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> IssueManagement.Common.Dashboard.Issue.UpdateIssueCategoryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueCategoryUpdate merchantShortId opCity apiTokenInfo issueCategoryId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueCategoryUpdate merchantShortId opCity apiTokenInfo issueCategoryId req

postIssueOptionCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage -> IssueManagement.Common.Dashboard.Issue.CreateIssueOptionReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.CreateIssueOptionRes)
postIssueOptionCreate merchantShortId opCity apiTokenInfo issueCategoryId issueMessageId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueOptionCreate merchantShortId opCity apiTokenInfo issueCategoryId issueMessageId req

postIssueOptionUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption -> IssueManagement.Common.Dashboard.Issue.UpdateIssueOptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueOptionUpdate merchantShortId opCity apiTokenInfo issueOptionid req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueOptionUpdate merchantShortId opCity apiTokenInfo issueOptionid req

postIssueMessageUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageRes)
postIssueMessageUpsert merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueMessageUpsert merchantShortId opCity apiTokenInfo req

getIssueCategoryDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueCategoryDetailRes)
getIssueCategoryDetail merchantShortId opCity apiTokenInfo categoryId language = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.getIssueCategoryDetail merchantShortId opCity apiTokenInfo categoryId language

getIssueOptionDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueOptionDetailRes)
getIssueOptionDetail merchantShortId opCity apiTokenInfo optionId language = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.getIssueOptionDetail merchantShortId opCity apiTokenInfo optionId language

getIssueMessageDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueMessageDetailRes)
getIssueMessageDetail merchantShortId opCity apiTokenInfo messageId language = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.getIssueMessageDetail merchantShortId opCity apiTokenInfo messageId language

getIssueMessageList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueMessageListRes)
getIssueMessageList merchantShortId opCity apiTokenInfo categoryId optionId isActive language = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.getIssueMessageList merchantShortId opCity apiTokenInfo categoryId optionId isActive language

getIssueOptionList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueOptionListDRes)
getIssueOptionList merchantShortId opCity apiTokenInfo categoryId messageId isActive language = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.getIssueOptionList merchantShortId opCity apiTokenInfo categoryId messageId isActive language

deleteIssueCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIssueCategory merchantShortId opCity apiTokenInfo categoryId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.deleteIssueCategory merchantShortId opCity apiTokenInfo categoryId

deleteIssueOption :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIssueOption merchantShortId opCity apiTokenInfo optionId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.deleteIssueOption merchantShortId opCity apiTokenInfo optionId

deleteIssueMessage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIssueMessage merchantShortId opCity apiTokenInfo messageId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.deleteIssueMessage merchantShortId opCity apiTokenInfo messageId

getIssueCategoryFlowPreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueCategoryFlowPreviewRes)
getIssueCategoryFlowPreview merchantShortId opCity apiTokenInfo categoryId language = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.getIssueCategoryFlowPreview merchantShortId opCity apiTokenInfo categoryId language

getIssueTranslations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueTranslationListRes)
getIssueTranslations merchantShortId opCity apiTokenInfo sentence = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.getIssueTranslations merchantShortId opCity apiTokenInfo sentence

postIssueBulkUpsertTranslations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.BulkUpsertTranslationsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueBulkUpsertTranslations merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueBulkUpsertTranslations merchantShortId opCity apiTokenInfo req

getIssueConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueConfigRes)
getIssueConfig merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.getIssueConfig merchantShortId opCity apiTokenInfo

postIssueConfigUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.UpdateIssueConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueConfigUpdate merchantShortId opCity apiTokenInfo req

postIssueCategoryReorder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.ReorderIssueCategoryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueCategoryReorder merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueCategoryReorder merchantShortId opCity apiTokenInfo req

postIssueOptionReorder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.ReorderIssueOptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueOptionReorder merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueOptionReorder merchantShortId opCity apiTokenInfo req

postIssueMessageReorder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.ReorderIssueMessageReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueMessageReorder merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueMessageReorder merchantShortId opCity apiTokenInfo req

postIssueCategoryCopy :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.CopyIssueCategoryReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.CopyIssueCategoryRes)
postIssueCategoryCopy merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueCategoryCopy merchantShortId opCity apiTokenInfo req

postIssueCategoryDefaultCopy :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.CopyAllIssueCategoryRes)
postIssueCategoryDefaultCopy merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueCategoryDefaultCopy merchantShortId opCity apiTokenInfo

postIssueCategoryAllCopy :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.CopyAllIssueCategoryReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.CopyAllIssueCategoryRes)
postIssueCategoryAllCopy merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueCategoryAllCopy merchantShortId opCity apiTokenInfo req

postIssueChatMessage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.SendChatMessageReq -> Environment.FlowHandler IssueManagement.Common.UI.Issue.ChatMessageItem)
postIssueChatMessage merchantShortId opCity apiTokenInfo issueId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueChatMessage merchantShortId opCity apiTokenInfo issueId req

getIssueChatMessages :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler [IssueManagement.Common.UI.Issue.ChatMessageItem])
getIssueChatMessages merchantShortId opCity apiTokenInfo issueId since limit = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.getIssueChatMessages merchantShortId opCity apiTokenInfo issueId since limit

postIssueChatRead :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.MarkChatReadByUserReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueChatRead merchantShortId opCity apiTokenInfo issueId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IssueManagement.Issue.postIssueChatRead merchantShortId opCity apiTokenInfo issueId req
