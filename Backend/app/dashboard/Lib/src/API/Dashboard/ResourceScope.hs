{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.ResourceScope where

import qualified Domain.Action.Dashboard.ResourceScope as DRS
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Id
import Kernel.Utils.Common (FlowHandlerR, FlowServerR)
import Servant hiding (throwError)
import Tools.Auth.Dashboard
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow, withDashboardDbFlowHandlerAPI)

-- Layer C surface. /user/resourceScope is what control-center analytics + the
-- frontend read to filter results/dropdowns to the caller's assigned resources;
-- /admin/person/{personId}/* is the management surface. Gated by DashboardAuth.

type API =
  "user"
    :> "resourceScope"
    :> DashboardAuth 'DASHBOARD_USER
    :> Get '[JSON] DRS.UserResourceScopeRes
    :<|> "admin"
      :> "person"
      :> ( DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "personId" (Id DP.Person)
             :> "resourceAccess"
             :> Get '[JSON] DRS.PersonResourceAccessRes
             :<|> DashboardAuth 'DASHBOARD_ADMIN
               :> Capture "personId" (Id DP.Person)
               :> "assignResourceAccess"
               :> ReqBody '[JSON] DRS.AssignResourceAccessReq
               :> Post '[JSON] APISuccess
             :<|> DashboardAuth 'DASHBOARD_ADMIN
               :> Capture "personId" (Id DP.Person)
               :> "resetResourceAccess"
               :> ReqBody '[JSON] DRS.ResetResourceAccessReq
               :> Post '[JSON] APISuccess
         )

handler :: DashboardLoginFlow (FlowR r) r => FlowServerR r API
handler =
  getUserResourceScope
    :<|> ( getPersonResourceAccess
             :<|> assignResourceAccess
             :<|> resetResourceAccess
         )

getUserResourceScope :: DashboardLoginFlow (FlowR r) r => TokenInfo -> FlowHandlerR r DRS.UserResourceScopeRes
getUserResourceScope = withDashboardDbFlowHandlerAPI . DRS.getUserResourceScope

getPersonResourceAccess :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> FlowHandlerR r DRS.PersonResourceAccessRes
getPersonResourceAccess tokenInfo = withDashboardDbFlowHandlerAPI . DRS.getPersonResourceAccess tokenInfo

assignResourceAccess :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> DRS.AssignResourceAccessReq -> FlowHandlerR r APISuccess
assignResourceAccess tokenInfo personId = withDashboardDbFlowHandlerAPI . DRS.assignResourceAccess tokenInfo personId

resetResourceAccess :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> DRS.ResetResourceAccessReq -> FlowHandlerR r APISuccess
resetResourceAccess tokenInfo personId = withDashboardDbFlowHandlerAPI . DRS.resetResourceAccess tokenInfo personId
