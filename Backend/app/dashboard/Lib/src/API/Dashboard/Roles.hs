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
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth.Dashboard
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow, withDashboardDbFlowHandlerAPI)

type API =
  "admin"
    :> "roles"
    :> ( "create"
           :> DashboardAuth 'DASHBOARD_ADMIN
           :> ReqBody '[JSON] DRoles.CreateRoleReq
           :> Post '[JSON] DRole.RoleAPIEntity
           :<|> "list"
             :> DashboardAuth 'DASHBOARD_ADMIN
             :> QueryParam "searchString" Text
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> Get '[JSON] DRoles.ListRoleRes
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "roleId" (Id DRole.Role)
             :> "disable"
             :> ReqBody '[JSON] DRoles.DisableRoleReq
             :> Post '[JSON] APISuccess
       )

handler :: DashboardLoginFlow (FlowR r) r => FlowServerR r API
handler =
  createRole
    :<|> listRoles
    :<|> disableRole

createRole :: DashboardLoginFlow (FlowR r) r => TokenInfo -> DRoles.CreateRoleReq -> FlowHandlerR r DRole.RoleAPIEntity
createRole tokenInfo =
  withDashboardDbFlowHandlerAPI . DRoles.createRole tokenInfo

listRoles :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandlerR r DRoles.ListRoleRes
listRoles mbsearchstr mblimit mboffset =
  withDashboardDbFlowHandlerAPI . DRoles.listRoles mbsearchstr mblimit mboffset

disableRole :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DRole.Role -> DRoles.DisableRoleReq -> FlowHandlerR r APISuccess
disableRole tokenInfo roleId =
  withDashboardDbFlowHandlerAPI . DRoles.disableRole tokenInfo roleId
