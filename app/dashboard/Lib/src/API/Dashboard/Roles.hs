module API.Dashboard.Roles where

import qualified Domain.Action.Dashboard.Roles as DRoles
import Domain.Types.Role as DRole
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
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
           :<|> "list"
             :> DashboardAuth 'DASHBOARD_ADMIN
             :> QueryParam "searchString" Text
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> Get '[JSON] DRoles.ListRoleRes
       )

handler :: FlowServer API
handler =
  createRole
    :<|> assignAccessLevel
    :<|> listRoles

createRole :: TokenInfo -> DRoles.CreateRoleReq -> FlowHandler DRole.RoleAPIEntity
createRole tokenInfo =
  withFlowHandlerAPI . DRoles.createRole tokenInfo

assignAccessLevel :: TokenInfo -> Id DRole.Role -> DRoles.AssignAccessLevelReq -> FlowHandler APISuccess
assignAccessLevel tokenInfo roleId =
  withFlowHandlerAPI . DRoles.assignAccessLevel tokenInfo roleId

listRoles :: TokenInfo -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler DRoles.ListRoleRes
listRoles mbsearchstr mblimit mboffset =
  withFlowHandlerAPI . DRoles.listRoles mbsearchstr mblimit mboffset
