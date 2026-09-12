{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.IssueManagement.IssueList
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.IssueManagement.IssueList
import qualified Data.Aeson
import qualified Domain.Action.Dashboard.IssueManagement.IssueList
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("issue" :> (GetIssueListV1 :<|> PostIssueListTicketStatusCallBack))

type GetIssueListV1 = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE_LIST/GET_ISSUE_LIST_V1" :> API.Types.RiderPlatform.IssueManagement.IssueList.GetIssueListV1)

type PostIssueListTicketStatusCallBack = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_ISSUE_MANAGEMENT/ISSUE_LIST/POST_ISSUE_LIST_TICKET_STATUS_CALL_BACK" :> API.Types.RiderPlatform.IssueManagement.IssueList.PostIssueListTicketStatusCallBack)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getIssueListV1 merchantId city :<|> postIssueListTicketStatusCallBack merchantId city

getIssueListV1 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.RiderPlatform.IssueManagement.IssueList.IssueListRes)
getIssueListV1 a9 a8 _a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.IssueList.getIssueListV1 a9 a8 a6 a5 a4 a3 a2 a1

postIssueListTicketStatusCallBack :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueListTicketStatusCallBack a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IssueManagement.IssueList.postIssueListTicketStatusCallBack a4 a3 a1
