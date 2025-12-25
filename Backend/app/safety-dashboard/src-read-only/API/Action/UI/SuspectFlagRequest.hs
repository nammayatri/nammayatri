{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SuspectFlagRequest
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SuspectFlagRequest
import qualified Data.Text
import qualified Domain.Action.UI.SuspectFlagRequest
import qualified Domain.Types.SuspectFlagRequest
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import "lib-dashboard" Tools.Auth

type API =
  ( DashboardAuth 'MERCHANT_MAKER :> "list" :> "suspectsFlag" :> QueryParam "approvalStatus" Domain.Types.SuspectFlagRequest.AdminApproval
      :> QueryParam
           "dl"
           Data.Text.Text
      :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "voterId"
           Data.Text.Text
      :> Get
           '[JSON]
           API.Types.UI.SuspectFlagRequest.SuspectFlagRequestList
      :<|> DashboardAuth 'MERCHANT_ADMIN
      :> "process"
      :> "suspectFlagRequest"
      :> ReqBody
           '[JSON]
           API.Types.UI.SuspectFlagRequest.SuspectApprovalReqList
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getListSuspectsFlag :<|> postProcessSuspectFlagRequest

getListSuspectsFlag :: (TokenInfo -> Kernel.Prelude.Maybe Domain.Types.SuspectFlagRequest.AdminApproval -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Data.Text.Text -> Environment.FlowHandler API.Types.UI.SuspectFlagRequest.SuspectFlagRequestList)
getListSuspectsFlag a8 a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SuspectFlagRequest.getListSuspectsFlag a8 a7 a6 a5 a4 a3 a2 a1

postProcessSuspectFlagRequest :: (TokenInfo -> API.Types.UI.SuspectFlagRequest.SuspectApprovalReqList -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postProcessSuspectFlagRequest a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SuspectFlagRequest.postProcessSuspectFlagRequest a2 a1
