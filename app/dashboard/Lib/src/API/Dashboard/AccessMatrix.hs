module API.Dashboard.AccessMatrix where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.Dashboard.AccessMatrix as DAccessMatrix
import Domain.Types.AccessMatrix as DMatrix
import Domain.Types.Person as DP
import Domain.Types.Role as DRole
import Environment
import Servant
import Tools.Auth
import Tools.Roles.Instances

type API =
  "accessMatrix"
    :> TokenAuth (DashboardAccessLevel 'DASHBOARD_ADMIN)
    :> QueryParam "roleId" (Id DRole.Role) -- role.name?
    :> Get '[JSON] DMatrix.AccessMatrixAPIEntity

handler :: FlowServer API
handler = getAccessMatrix

getAccessMatrix :: Id DP.Person -> Maybe (Id DRole.Role) -> FlowHandler AccessMatrixAPIEntity
getAccessMatrix personId =
  withFlowHandlerAPI . DAccessMatrix.getAccessMatrix personId
