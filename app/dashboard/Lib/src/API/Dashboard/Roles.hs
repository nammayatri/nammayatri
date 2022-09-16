module API.Dashboard.Roles where

import Beckn.Prelude
import Beckn.Types.APISuccess
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.Dashboard.Roles as DRoles
import Domain.Types.Person as DP
import Domain.Types.Role as DRole
import Environment
import Servant
import Tools.Auth

type API =
  "roles"
    :> ( "create"
           :> TokenAuth (DashboardAccessLevel 'DASHBOARD_ADMIN)
           :> ReqBody '[JSON] DRoles.CreateRoleReq
           :> Post '[JSON] DRole.RoleAPIEntity
           :<|> TokenAuth (DashboardAccessLevel 'DASHBOARD_ADMIN)
           :> Capture "roleId" (Id DRole.Role)
           :> "assignAccessLevel"
           :> ReqBody '[JSON] DRoles.AssignAccessLevelReq
           :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  createRole
    :<|> assignAccessLevel

createRole :: Id DP.Person -> DRoles.CreateRoleReq -> FlowHandler DRole.RoleAPIEntity
createRole adminId =
  withFlowHandlerAPI . DRoles.createRole adminId

assignAccessLevel :: Id DP.Person -> Id DRole.Role -> DRoles.AssignAccessLevelReq -> FlowHandler APISuccess
assignAccessLevel adminId roleId =
  withFlowHandlerAPI . DRoles.assignAccessLevel adminId roleId
