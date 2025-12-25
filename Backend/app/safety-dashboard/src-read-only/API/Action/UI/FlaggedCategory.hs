{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FlaggedCategory
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FlaggedCategory
import qualified Domain.Action.UI.FlaggedCategory
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import "lib-dashboard" Tools.Auth

type API =
  ( DashboardAuth 'DASHBOARD_ADMIN :> "add" :> "flagCategory" :> ReqBody '[JSON] API.Types.UI.FlaggedCategory.AddFlagCategoryReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> DashboardAuth 'DASHBOARD_ADMIN
      :> "delete"
      :> "flagCategory"
      :> ReqBody
           '[JSON]
           API.Types.UI.FlaggedCategory.DeleteFlagCategoryReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> DashboardAuth 'DASHBOARD_USER
      :> "list"
      :> "flagCategory"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           API.Types.UI.FlaggedCategory.FlagCategoryList
  )

handler :: Environment.FlowServer API
handler = postAddFlagCategory :<|> postDeleteFlagCategory :<|> getListFlagCategory

postAddFlagCategory :: (TokenInfo -> API.Types.UI.FlaggedCategory.AddFlagCategoryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postAddFlagCategory a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.FlaggedCategory.postAddFlagCategory a2 a1

postDeleteFlagCategory :: (TokenInfo -> API.Types.UI.FlaggedCategory.DeleteFlagCategoryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDeleteFlagCategory a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.FlaggedCategory.postDeleteFlagCategory a2 a1

getListFlagCategory :: (TokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.UI.FlaggedCategory.FlagCategoryList)
getListFlagCategory a3 a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.FlaggedCategory.getListFlagCategory a3 a2 a1
