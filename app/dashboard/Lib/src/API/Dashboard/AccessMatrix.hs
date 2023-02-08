module API.Dashboard.AccessMatrix where

import qualified Domain.Action.Dashboard.AccessMatrix as DAccessMatrix
import Domain.Types.AccessMatrix as DMatrix
import Domain.Types.Role as DRole
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "admin"
    :> "accessMatrix"
    :> ( DashboardAuth 'DASHBOARD_ADMIN
           :> QueryParam "limit" Integer
           :> QueryParam "offset" Integer
           :> Get '[JSON] DMatrix.AccessMatrixAPIEntity
           :<|> "role"
             :> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "roleId" (Id DRole.Role) -- role.name?
             :> Get '[JSON] DMatrix.AccessMatrixRowAPIEntity
       )

handler :: FlowServer API
handler = getAccessMatrix :<|> getAccessMatrixByRole

getAccessMatrix :: TokenInfo -> Maybe Integer -> Maybe Integer -> FlowHandler AccessMatrixAPIEntity
getAccessMatrix tokenInfo mbLimit =
  withFlowHandlerAPI . DAccessMatrix.getAccessMatrix tokenInfo mbLimit

getAccessMatrixByRole :: TokenInfo -> Id DRole.Role -> FlowHandler AccessMatrixRowAPIEntity
getAccessMatrixByRole tokenInfo =
  withFlowHandlerAPI . DAccessMatrix.getAccessMatrixByRole tokenInfo
