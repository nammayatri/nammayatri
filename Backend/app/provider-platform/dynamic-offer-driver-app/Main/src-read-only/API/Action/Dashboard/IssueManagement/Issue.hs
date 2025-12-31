{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.IssueManagement.Issue
  ( API.Types.ProviderPlatform.IssueManagement.Issue.API,
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
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.IssueManagement.Issue.API)
handler merchantId city = getIssueCategoryList merchantId city :<|> getIssueList merchantId city :<|> getIssueInfo merchantId city :<|> getIssueInfoV2 merchantId city :<|> putIssueUpdate merchantId city :<|> postIssueComment merchantId city :<|> getIssueMedia merchantId city :<|> postIssueTicketStatusCallBack merchantId city :<|> postIssueCategoryCreate merchantId city :<|> postIssueCategoryUpdate merchantId city :<|> postIssueOptionCreate merchantId city :<|> postIssueOptionUpdate merchantId city :<|> postIssueMessageUpsert merchantId city

getIssueCategoryList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueCategoryListRes)
getIssueCategoryList a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueCategoryList a2 a1

getIssueList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe IssueManagement.Common.IssueStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueReportListResponse)
getIssueList a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueList a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getIssueInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueInfoDRes)
getIssueInfo a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueInfo a3 a2 a1

getIssueInfoV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId IssueManagement.Domain.Types.Issue.IssueReport.IssueReport) -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.IssueInfoDRes)
getIssueInfoV2 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueInfoV2 a4 a3 a2 a1

putIssueUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.IssueUpdateByUserReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIssueUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.putIssueUpdate a4 a3 a2 a1

postIssueComment :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.IssueAddCommentByUserReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueComment a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueComment a4 a3 a2 a1

getIssueMedia :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Prelude.Text)
getIssueMedia a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.getIssueMedia a3 a2 a1

postIssueTicketStatusCallBack :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueTicketStatusCallBack a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueTicketStatusCallBack a3 a2 a1

postIssueCategoryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.CreateIssueCategoryRes)
postIssueCategoryCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueCategoryCreate a3 a2 a1

postIssueCategoryUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> IssueManagement.Common.Dashboard.Issue.UpdateIssueCategoryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueCategoryUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueCategoryUpdate a4 a3 a2 a1

postIssueOptionCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueCategory.IssueCategory -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueMessage.IssueMessage -> IssueManagement.Common.Dashboard.Issue.CreateIssueOptionReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.CreateIssueOptionRes)
postIssueOptionCreate a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueOptionCreate a5 a4 a3 a2 a1

postIssueOptionUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueOption.IssueOption -> IssueManagement.Common.Dashboard.Issue.UpdateIssueOptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueOptionUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueOptionUpdate a4 a3 a2 a1

postIssueMessageUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageReq -> Environment.FlowHandler IssueManagement.Common.Dashboard.Issue.UpsertIssueMessageRes)
postIssueMessageUpsert a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.Issue.postIssueMessageUpsert a3 a2 a1
