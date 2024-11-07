{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.IssueManagement.IssueList
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.IssueManagement
import qualified API.Types.RiderPlatform.IssueManagement.IssueList
import qualified Data.Aeson
import qualified Domain.Action.RiderPlatform.IssueManagement.IssueList
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("issue" :> (GetIssueListV1 :<|> PostIssueListTicketStatusCallBack))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getIssueListV1 merchantId city :<|> postIssueListTicketStatusCallBack merchantId city

type GetIssueListV1 =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE_LIST / 'API.Types.RiderPlatform.IssueManagement.IssueList.GET_ISSUE_LIST_V1)
      :> API.Types.RiderPlatform.IssueManagement.IssueList.GetIssueListV1
  )

type PostIssueListTicketStatusCallBack =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_ISSUE_MANAGEMENT / 'API.Types.RiderPlatform.IssueManagement.ISSUE_LIST / 'API.Types.RiderPlatform.IssueManagement.IssueList.POST_ISSUE_LIST_TICKET_STATUS_CALL_BACK)
      :> API.Types.RiderPlatform.IssueManagement.IssueList.PostIssueListTicketStatusCallBack
  )

getIssueListV1 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.RiderPlatform.IssueManagement.IssueList.IssueListRes)
getIssueListV1 merchantShortId opCity apiTokenInfo limit offset mobileCountryCode mobileNumber from to = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.IssueList.getIssueListV1 merchantShortId opCity apiTokenInfo limit offset mobileCountryCode mobileNumber from to

postIssueListTicketStatusCallBack :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Aeson.Value -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIssueListTicketStatusCallBack merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IssueManagement.IssueList.postIssueListTicketStatusCallBack merchantShortId opCity apiTokenInfo req
