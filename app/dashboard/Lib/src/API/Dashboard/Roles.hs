module API.Dashboard.Roles where

import Beckn.Prelude
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
       )

handler :: FlowServer API
handler = createRole

createRole :: Id DP.Person -> DRoles.CreateRoleReq -> FlowHandler DRole.RoleAPIEntity
createRole personId =
  withFlowHandlerAPI . DRoles.createRole personId
