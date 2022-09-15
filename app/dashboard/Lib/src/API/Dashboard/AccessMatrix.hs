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

type API =
  "accessMatrix"
    :> ( TokenAuth (DashboardAccessLevel 'DASHBOARD_ADMIN)
           :> QueryParam "limit" Integer
           :> QueryParam "offset" Integer
           :> Get '[JSON] DMatrix.AccessMatrixAPIEntity
           :<|> "role"
           :> TokenAuth (DashboardAccessLevel 'DASHBOARD_ADMIN)
           :> Capture "roleId" (Id DRole.Role) -- role.name?
           :> Get '[JSON] DMatrix.AccessMatrixRowAPIEntity
       )

handler :: FlowServer API
handler = getAccessMatrix :<|> getAccessMatrixByRole

getAccessMatrix :: Id DP.Person -> Maybe Integer -> Maybe Integer -> FlowHandler AccessMatrixAPIEntity
getAccessMatrix adminId mbLimit =
  withFlowHandlerAPI . DAccessMatrix.getAccessMatrix adminId mbLimit

getAccessMatrixByRole :: Id DP.Person -> Id DRole.Role -> FlowHandler AccessMatrixRowAPIEntity
getAccessMatrixByRole adminId =
  withFlowHandlerAPI . DAccessMatrix.getAccessMatrixByRole adminId
