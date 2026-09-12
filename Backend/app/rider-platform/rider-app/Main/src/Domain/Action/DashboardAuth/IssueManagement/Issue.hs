{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handlers for direct-dashboard issue routes whose request names
-- the acting operator.
--
-- The Helper requests (@*ByUserReq@) carry the operator's id, which
-- provider-dashboard filled from its own session before forwarding the call.
-- The generated @API.Action.DashboardAuth@ handler serves the public request and
-- calls these functions for every endpoint marked @appServerHandler: custom@ in
-- the spec, so the id always comes from the verified session, never the client.
module Domain.Action.DashboardAuth.IssueManagement.Issue
  ( putIssueUpdate,
    postIssueComment,
    postIssueChatMessage,
  )
where

import qualified Domain.Action.Dashboard.IssueManagement.Issue
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified IssueManagement.Common
import qualified IssueManagement.Common.Dashboard.Issue
import qualified IssueManagement.Common.UI.Issue
import qualified IssueManagement.Domain.Types.Issue.IssueReport
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Tools.Auth.DashboardUserAuth

putIssueUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.IssueUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putIssueUpdate a5 a4 a3 a2 IssueManagement.Common.Dashboard.Issue.IssueUpdateReq {..} =
  Domain.Action.Dashboard.IssueManagement.Issue.putIssueUpdate a5 a4 a2 IssueManagement.Common.Dashboard.Issue.IssueUpdateByUserReq {userId = operatorUserId a3, ..}

postIssueComment :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.IssueAddCommentReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postIssueComment a5 a4 a3 a2 IssueManagement.Common.Dashboard.Issue.IssueAddCommentReq {..} =
  Domain.Action.Dashboard.IssueManagement.Issue.postIssueComment a5 a4 a2 IssueManagement.Common.Dashboard.Issue.IssueAddCommentByUserReq {userId = operatorUserId a3, ..}

postIssueChatMessage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport -> IssueManagement.Common.Dashboard.Issue.SendChatMessageReq -> Environment.Flow IssueManagement.Common.UI.Issue.ChatMessageItem)
postIssueChatMessage a5 a4 a3 a2 IssueManagement.Common.Dashboard.Issue.SendChatMessageReq {..} =
  Domain.Action.Dashboard.IssueManagement.Issue.postIssueChatMessage a5 a4 a2 IssueManagement.Common.Dashboard.Issue.SendChatMessageByUserReq {userId = operatorUserId a3, ..}

operatorUserId :: DashboardUser -> Kernel.Types.Id.Id IssueManagement.Common.User
operatorUserId = Kernel.Types.Id.Id . Tools.Auth.DashboardUserAuth.dashboardRequestorId
