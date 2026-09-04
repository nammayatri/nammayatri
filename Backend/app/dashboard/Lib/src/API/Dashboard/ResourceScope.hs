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
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI')
import Servant hiding (throwError)
import Storage.Beam.BeamFlow
import Tools.Auth

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

handler :: BeamFlow' => FlowServer API
handler =
  getUserResourceScope
    :<|> ( getPersonResourceAccess
             :<|> assignResourceAccess
             :<|> resetResourceAccess
         )

getUserResourceScope :: BeamFlow' => TokenInfo -> FlowHandler DRS.UserResourceScopeRes
getUserResourceScope = withFlowHandlerAPI' . DRS.getUserResourceScope

getPersonResourceAccess :: BeamFlow' => TokenInfo -> Id DP.Person -> FlowHandler DRS.PersonResourceAccessRes
getPersonResourceAccess tokenInfo = withFlowHandlerAPI' . DRS.getPersonResourceAccess tokenInfo

assignResourceAccess :: BeamFlow' => TokenInfo -> Id DP.Person -> DRS.AssignResourceAccessReq -> FlowHandler APISuccess
assignResourceAccess tokenInfo personId = withFlowHandlerAPI' . DRS.assignResourceAccess tokenInfo personId

resetResourceAccess :: BeamFlow' => TokenInfo -> Id DP.Person -> DRS.ResetResourceAccessReq -> FlowHandler APISuccess
resetResourceAccess tokenInfo personId = withFlowHandlerAPI' . DRS.resetResourceAccess tokenInfo personId
