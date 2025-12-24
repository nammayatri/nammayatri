{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.IssueManagement.Issue
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.IssueManagement
import qualified API.Types.RiderPlatform.IssueManagement.Issue
import qualified Data.Aeson
import qualified Domain.Action.RiderPlatform.IssueManagement.Issue
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified IssueManagement.Common
import qualified IssueManagement.Common.Dashboard.Issue
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

type API = ("issueV2" :> (GetIssueCategoryList :<|> GetIssueList :<|> GetIssueInfo :<|> GetIssueInfoV2 :<|> PutIssueUpdate :<|> PostIssueComment :<|> GetIssueMedia :<|> PostIssueTicketStatusCallBack :<|> PostIssueCategoryCreate :<|> PostIssueCategoryUpdate :<|> PostIssueOptionCreate :<|> PostIssueOptionUpdate :<|> PostIssueMessageUpsert :<|> PostIssueKaptureCreate :<|> GetIssueCategoryDetail :<|> GetIssueOptionDetail :<|> GetIssueMessageDetail :<|> GetIssueMessageList :<|> GetIssueOptionList :<|> DeleteIssueCategory :<|> DeleteIssueOption :<|> DeleteIssueMessage :<|> GetIssueCategoryFlowPreview :<|> GetIssueTranslations :<|> PostIssueBulkUpsertTranslations :<|> GetIssueConfig :<|> PostIssueConfigUpdate :<|> PostIssueCategoryReorder :<|> PostIssueOptionReorder :<|> PostIssueMessageReorder))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getIssueCategoryList merchantId city :<|> getIssueList merchantId city :<|> getIssueInfo merchantId city :<|> getIssueInfoV2 merchantId city :<|> putIssueUpdate merchantId city :<|> postIssueComment merchantId city :<|> getIssueMedia merchantId city :<|> postIssueTicketStatusCallBack merchantId city :<|> postIssueCategoryCreate merchantId city :<|> postIssueCategoryUpdate merchantId city :<|> postIssueOptionCreate merchantId city :<|> postIssueOptionUpdate merchantId city :<|> postIssueMessageUpsert merchantId city :<|> postIssueKaptureCreate merchantId city :<|> getIssueCategoryDetail merchantId city :<|> getIssueOptionDetail merchantId city :<|> getIssueMessageDetail merchantId city :<|> getIssueMessageList merchantId city :<|> getIssueOptionList merchantId city :<|> deleteIssueCategory merchantId city :<|> deleteIssueOption merchantId city :<|> deleteIssueMessage merchantId city :<|> getIssueCategoryFlowPreview merchantId city :<|> getIssueTranslations merchantId city :<|> postIssueBulkUpsertTranslations merchantId city :<|> getIssueConfig merchantId city :<|> postIssueConfigUpdate merchantId city :<|> postIssueCategoryReorder merchantId city :<|> postIssueOptionReorder merchantId city :<|> postIssueMessageReorder merchantId city

type GetIssueCategoryList =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_ISSUE_MANAGEMENT) / ('API.Types.RiderPlatform.IssueManagement.ISSUE) / ('API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_CATEGORY_LIST))
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueCategoryList
  )

type GetIssueList =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_ISSUE_MANAGEMENT) / ('API.Types.RiderPlatform.IssueManagement.ISSUE) / ('API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_LIST))
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueList
  )

type GetIssueInfo =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_ISSUE_MANAGEMENT) / ('API.Types.RiderPlatform.IssueManagement.ISSUE) / ('API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_INFO))
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueInfo
  )

type GetIssueInfoV2 =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_ISSUE_MANAGEMENT) / ('API.Types.RiderPlatform.IssueManagement.ISSUE) / ('API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_INFO_V2))
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueInfoV2
  )

type PutIssueUpdate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_ISSUE_MANAGEMENT) / ('API.Types.RiderPlatform.IssueManagement.ISSUE) / ('API.Types.RiderPlatform.IssueManagement.Issue.PUT_ISSUE_UPDATE))
      :> API.Types.RiderPlatform.IssueManagement.Issue.PutIssueUpdate
  )

type PostIssueComment =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_ISSUE_MANAGEMENT) / ('API.Types.RiderPlatform.IssueManagement.ISSUE) / ('API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_COMMENT))
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueComment
  )

type GetIssueMedia =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_ISSUE_MANAGEMENT) / ('API.Types.RiderPlatform.IssueManagement.ISSUE) / ('API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_MEDIA))
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueMedia
  )

type PostIssueTicketStatusCallBack =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_ISSUE_MANAGEMENT) / ('API.Types.RiderPlatform.IssueManagement.ISSUE) / ('API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_TICKET_STATUS_CALL_BACK))
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueTicketStatusCallBack
  )

type PostIssueCategoryCreate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_ISSUE_MANAGEMENT) / ('API.Types.RiderPlatform.IssueManagement.ISSUE) / ('API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_CATEGORY_CREATE))
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueCategoryCreate
  )

type PostIssueCategoryUpdate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_ISSUE_MANAGEMENT) / ('API.Types.RiderPlatform.IssueManagement.ISSUE) / ('API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_CATEGORY_UPDATE))
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueCategoryUpdate
  )

type PostIssueOptionCreate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_ISSUE_MANAGEMENT) / ('API.Types.RiderPlatform.IssueManagement.ISSUE) / ('API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_OPTION_CREATE))
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueOptionCreate
  )

type PostIssueOptionUpdate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_ISSUE_MANAGEMENT) / ('API.Types.RiderPlatform.IssueManagement.ISSUE) / ('API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_OPTION_UPDATE))
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueOptionUpdate
  )

type PostIssueMessageUpsert =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_ISSUE_MANAGEMENT) / ('API.Types.RiderPlatform.IssueManagement.ISSUE) / ('API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_MESSAGE_UPSERT))
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueMessageUpsert
  )

type PostIssueKaptureCreate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_ISSUE_MANAGEMENT) / ('API.Types.RiderPlatform.IssueManagement.ISSUE) / ('API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_KAPTURE_CREATE))
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueKaptureCreate
  )

type GetIssueCategoryDetail =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_CATEGORY_DETAIL)
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueCategoryDetail
  )

type GetIssueOptionDetail =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_OPTION_DETAIL)
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueOptionDetail
  )

type GetIssueMessageDetail =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_MESSAGE_DETAIL)
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueMessageDetail
  )

type GetIssueMessageList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_MESSAGE_LIST)
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueMessageList
  )

type GetIssueOptionList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_OPTION_LIST)
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueOptionList
  )

type DeleteIssueCategory =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.DELETE_ISSUE_CATEGORY)
      :> API.Types.RiderPlatform.IssueManagement.Issue.DeleteIssueCategory
  )

type DeleteIssueOption =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.DELETE_ISSUE_OPTION)
      :> API.Types.RiderPlatform.IssueManagement.Issue.DeleteIssueOption
  )

type DeleteIssueMessage =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.DELETE_ISSUE_MESSAGE)
      :> API.Types.RiderPlatform.IssueManagement.Issue.DeleteIssueMessage
  )

type GetIssueCategoryFlowPreview =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_CATEGORY_FLOW_PREVIEW)
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueCategoryFlowPreview
  )

type GetIssueTranslations =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_TRANSLATIONS)
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueTranslations
  )

type PostIssueBulkUpsertTranslations =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_BULK_UPSERT_TRANSLATIONS)
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueBulkUpsertTranslations
  )

type GetIssueConfig =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_CONFIG)
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueConfig
  )

type PostIssueConfigUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_CONFIG_UPDATE)
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueConfigUpdate
  )

type PostIssueCategoryReorder =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_CATEGORY_REORDER)
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueCategoryReorder
  )

type PostIssueOptionReorder =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_OPTION_REORDER)
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueOptionReorder
  )

type PostIssueMessageReorder =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_MESSAGE_REORDER)
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueMessageReorder
  )

getIssueCategoryList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueCategoryListRes)
getIssueCategoryList merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueCategoryList merchantShortId opCity apiTokenInfo

getIssueList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (IssueManagement.Common.IssueStatus) -> Kernel.Prelude.Maybe ((Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory)) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Common.Ride) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueReportListResponse)
getIssueList merchantShortId opCity apiTokenInfo limit offset status category categoryName assignee countryCode phoneNumber rideShortId descriptionSearch = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueList merchantShortId opCity apiTokenInfo limit offset status category categoryName assignee countryCode phoneNumber rideShortId descriptionSearch

getIssueInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueInfoRes)
getIssueInfo merchantShortId opCity apiTokenInfo issueId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueInfo merchantShortId opCity apiTokenInfo issueId

getIssueInfoV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe ((Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport)) -> Kernel.Prelude.Maybe ((Kernel.Types.Id.ShortId IssueManagement.Domain.Types.Issue.IssueReport.IssueReport)) -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueInfoRes)
getIssueInfoV2 merchantShortId opCity apiTokenInfo issueId issueShortId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueInfoV2 merchantShortId opCity apiTokenInfo issueId issueShortId

putIssueUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.IssueUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIssueUpdate merchantShortId opCity apiTokenInfo issueId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.putIssueUpdate merchantShortId opCity apiTokenInfo issueId req

postIssueComment :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.IssueAddCommentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueComment merchantShortId opCity apiTokenInfo issueId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.postIssueComment merchantShortId opCity apiTokenInfo issueId req

getIssueMedia :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Prelude.Text)
getIssueMedia merchantShortId opCity apiTokenInfo filePath = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueMedia merchantShortId opCity apiTokenInfo filePath

postIssueTicketStatusCallBack :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueTicketStatusCallBack merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.postIssueTicketStatusCallBack merchantShortId opCity apiTokenInfo req

postIssueCategoryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryRes)
postIssueCategoryCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.postIssueCategoryCreate merchantShortId opCity apiTokenInfo req

postIssueCategoryUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> IssueManagement.Common.Dashboard.Issue.UpdateIssueCategoryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueCategoryUpdate merchantShortId opCity apiTokenInfo issueCategoryId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.postIssueCategoryUpdate merchantShortId opCity apiTokenInfo issueCategoryId req

postIssueOptionCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage -> IssueManagement.Common.Dashboard.Issue.CreateIssueOptionReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.CreateIssueOptionRes)
postIssueOptionCreate merchantShortId opCity apiTokenInfo issueCategoryId issueMessageId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.postIssueOptionCreate merchantShortId opCity apiTokenInfo issueCategoryId issueMessageId req

postIssueOptionUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption -> IssueManagement.Common.Dashboard.Issue.UpdateIssueOptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueOptionUpdate merchantShortId opCity apiTokenInfo issueOptionid req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.postIssueOptionUpdate merchantShortId opCity apiTokenInfo issueOptionid req

postIssueMessageUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageRes)
postIssueMessageUpsert merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.postIssueMessageUpsert merchantShortId opCity apiTokenInfo req

postIssueKaptureCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.IssueReportReqV2 -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueKaptureCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.postIssueKaptureCreate merchantShortId opCity apiTokenInfo req

getIssueCategoryDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueCategoryDetailRes)
getIssueCategoryDetail merchantShortId opCity apiTokenInfo categoryId language = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueCategoryDetail merchantShortId opCity apiTokenInfo categoryId language

getIssueOptionDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueOptionDetailRes)
getIssueOptionDetail merchantShortId opCity apiTokenInfo optionId language = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueOptionDetail merchantShortId opCity apiTokenInfo optionId language

getIssueMessageDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueMessageDetailRes)
getIssueMessageDetail merchantShortId opCity apiTokenInfo messageId language = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueMessageDetail merchantShortId opCity apiTokenInfo messageId language

getIssueMessageList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueMessageListRes)
getIssueMessageList merchantShortId opCity apiTokenInfo categoryId optionId isActive language = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueMessageList merchantShortId opCity apiTokenInfo categoryId optionId isActive language

getIssueOptionList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueOptionListRes)
getIssueOptionList merchantShortId opCity apiTokenInfo categoryId messageId isActive language = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueOptionList merchantShortId opCity apiTokenInfo categoryId messageId isActive language

deleteIssueCategory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIssueCategory merchantShortId opCity apiTokenInfo categoryId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.deleteIssueCategory merchantShortId opCity apiTokenInfo categoryId

deleteIssueOption :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIssueOption merchantShortId opCity apiTokenInfo optionId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.deleteIssueOption merchantShortId opCity apiTokenInfo optionId

deleteIssueMessage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIssueMessage merchantShortId opCity apiTokenInfo messageId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.deleteIssueMessage merchantShortId opCity apiTokenInfo messageId

getIssueCategoryFlowPreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueCategoryFlowPreviewRes)
getIssueCategoryFlowPreview merchantShortId opCity apiTokenInfo categoryId language = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueCategoryFlowPreview merchantShortId opCity apiTokenInfo categoryId language

getIssueTranslations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueTranslationListRes)
getIssueTranslations merchantShortId opCity apiTokenInfo sentence = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueTranslations merchantShortId opCity apiTokenInfo sentence

postIssueBulkUpsertTranslations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.BulkUpsertTranslationsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueBulkUpsertTranslations merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.postIssueBulkUpsertTranslations merchantShortId opCity apiTokenInfo req

getIssueConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueConfigRes)
getIssueConfig merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueConfig merchantShortId opCity apiTokenInfo

postIssueConfigUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.UpdateIssueConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.postIssueConfigUpdate merchantShortId opCity apiTokenInfo req

postIssueCategoryReorder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.ReorderIssueCategoryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueCategoryReorder merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.postIssueCategoryReorder merchantShortId opCity apiTokenInfo req

postIssueOptionReorder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.ReorderIssueOptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueOptionReorder merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.postIssueOptionReorder merchantShortId opCity apiTokenInfo req

postIssueMessageReorder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> IssueManagement.Common.Dashboard.Issue.ReorderIssueMessageReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueMessageReorder merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.postIssueMessageReorder merchantShortId opCity apiTokenInfo req
