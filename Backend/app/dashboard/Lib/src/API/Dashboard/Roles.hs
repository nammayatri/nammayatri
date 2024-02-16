{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Roles where

import qualified Domain.Action.Dashboard.Roles as DRoles
import Domain.Types.Role as DRole
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.BeamFlow
import Tools.Auth

type API =
  "admin"
    :> "roles"
    :> ( "create"
           :> DashboardAuth 'DASHBOARD_ADMIN
           :> ReqBody '[JSON] DRoles.CreateRoleReq
           :> Post '[JSON] DRole.RoleAPIEntity
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "roleId" (Id DRole.Role)
             :> "assignAccessLevel"
             :> ReqBody '[JSON] DRoles.AssignAccessLevelReq
             :> Post '[JSON] APISuccess
           :<|> "list"
             :> DashboardAuth 'DASHBOARD_ADMIN
             :> QueryParam "searchString" Text
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> Get '[JSON] DRoles.ListRoleRes
       )

handler :: BeamFlow' => FlowServer API
handler =
  createRole
    :<|> assignAccessLevel
    :<|> listRoles

createRole :: BeamFlow' => TokenInfo -> DRoles.CreateRoleReq -> FlowHandler DRole.RoleAPIEntity
createRole tokenInfo =
  withFlowHandlerAPI' . DRoles.createRole tokenInfo

assignAccessLevel :: BeamFlow' => TokenInfo -> Id DRole.Role -> DRoles.AssignAccessLevelReq -> FlowHandler APISuccess
assignAccessLevel tokenInfo roleId =
  withFlowHandlerAPI' . DRoles.assignAccessLevel tokenInfo roleId

listRoles :: BeamFlow' => TokenInfo -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler DRoles.ListRoleRes
listRoles mbsearchstr mblimit mboffset =
  withFlowHandlerAPI' . DRoles.listRoles mbsearchstr mblimit mboffset
