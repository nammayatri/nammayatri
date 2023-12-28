{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
           :<|> "merchantWithCityList"
             :> Get '[JSON] [DMatrix.MerchantCityList]
       )

handler :: FlowServer API
handler =
  getAccessMatrix
    :<|> getAccessMatrixByRole
    :<|> getMerchantWithCityList

getAccessMatrix :: TokenInfo -> Maybe Integer -> Maybe Integer -> FlowHandler AccessMatrixAPIEntity
getAccessMatrix tokenInfo mbLimit =
  withFlowHandlerAPI' . DAccessMatrix.getAccessMatrix tokenInfo mbLimit

getAccessMatrixByRole :: TokenInfo -> Id DRole.Role -> FlowHandler AccessMatrixRowAPIEntity
getAccessMatrixByRole tokenInfo =
  withFlowHandlerAPI' . DAccessMatrix.getAccessMatrixByRole tokenInfo

getMerchantWithCityList :: FlowHandler [DMatrix.MerchantCityList]
getMerchantWithCityList =
  withFlowHandlerAPI' DAccessMatrix.getMerchantWithCityList
