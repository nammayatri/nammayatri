module API.Dashboard.Roles where

import Beckn.Prelude
import Beckn.Types.APISuccess
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.Dashboard.Roles as DRoles
import Domain.Types.Role as DRole
import Environment
import Servant
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
       )

handler :: FlowServer API
handler =
  createRole
    :<|> assignAccessLevel

createRole :: TokenInfo -> DRoles.CreateRoleReq -> FlowHandler DRole.RoleAPIEntity
createRole tokenInfo =
  withFlowHandlerAPI . DRoles.createRole tokenInfo

assignAccessLevel :: TokenInfo -> Id DRole.Role -> DRoles.AssignAccessLevelReq -> FlowHandler APISuccess
assignAccessLevel tokenInfo roleId =
  withFlowHandlerAPI . DRoles.assignAccessLevel tokenInfo roleId
