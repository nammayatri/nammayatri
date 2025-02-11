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
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("issueV2" :> (GetIssueCategoryList :<|> GetIssueList :<|> GetIssueInfo :<|> GetIssueInfoV2 :<|> PutIssueUpdate :<|> PostIssueComment :<|> GetIssueMedia :<|> PostIssueTicketStatusCallBack :<|> PostIssueCategoryCreate :<|> PostIssueCategoryUpdate :<|> PostIssueOptionCreate :<|> PostIssueOptionUpdate :<|> PostIssueMessageUpsert))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getIssueCategoryList merchantId city :<|> getIssueList merchantId city :<|> getIssueInfo merchantId city :<|> getIssueInfoV2 merchantId city :<|> putIssueUpdate merchantId city :<|> postIssueComment merchantId city :<|> getIssueMedia merchantId city :<|> postIssueTicketStatusCallBack merchantId city :<|> postIssueCategoryCreate merchantId city :<|> postIssueCategoryUpdate merchantId city :<|> postIssueOptionCreate merchantId city :<|> postIssueOptionUpdate merchantId city :<|> postIssueMessageUpsert merchantId city

type GetIssueCategoryList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_CATEGORY_LIST)
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueCategoryList
  )

type GetIssueList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_LIST)
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueList
  )

type GetIssueInfo =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_INFO)
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueInfo
  )

type GetIssueInfoV2 =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_INFO_V2)
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueInfoV2
  )

type PutIssueUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.PUT_ISSUE_UPDATE)
      :> API.Types.RiderPlatform.IssueManagement.Issue.PutIssueUpdate
  )

type PostIssueComment =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_COMMENT)
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueComment
  )

type GetIssueMedia =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.GET_ISSUE_MEDIA)
      :> API.Types.RiderPlatform.IssueManagement.Issue.GetIssueMedia
  )

type PostIssueTicketStatusCallBack =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_TICKET_STATUS_CALL_BACK)
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueTicketStatusCallBack
  )

type PostIssueCategoryCreate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_CATEGORY_CREATE)
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueCategoryCreate
  )

type PostIssueCategoryUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_CATEGORY_UPDATE)
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueCategoryUpdate
  )

type PostIssueOptionCreate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_OPTION_CREATE)
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueOptionCreate
  )

type PostIssueOptionUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_OPTION_UPDATE)
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueOptionUpdate
  )

type PostIssueMessageUpsert =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE / 'API.Types.RiderPlatform.IssueManagement.Issue.POST_ISSUE_MESSAGE_UPSERT)
      :> API.Types.RiderPlatform.IssueManagement.Issue.PostIssueMessageUpsert
  )

getIssueCategoryList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueCategoryListRes)
getIssueCategoryList merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueCategoryList merchantShortId opCity apiTokenInfo

getIssueList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe IssueManagement.Common.IssueStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Common.Ride) -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueReportListResponse)
getIssueList merchantShortId opCity apiTokenInfo limit offset status category assignee countryCode phoneNumber rideShortId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueList merchantShortId opCity apiTokenInfo limit offset status category assignee countryCode phoneNumber rideShortId

getIssueInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueInfoRes)
getIssueInfo merchantShortId opCity apiTokenInfo issueId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.Issue.getIssueInfo merchantShortId opCity apiTokenInfo issueId

getIssueInfoV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueInfoRes)
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
