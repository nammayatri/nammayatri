{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Registration where

import qualified Domain.Action.Dashboard.Registration as DReg
import Environment
import Kernel.Prelude
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "user"
    :> ( "login"
           :> ReqBody '[JSON] DReg.LoginReq
           :> Post '[JSON] DReg.LoginRes
           :<|> "logout"
             :> DashboardAuth 'DASHBOARD_USER
             :> Post '[JSON] DReg.LogoutRes
           :<|> "logoutAllMerchants"
             :> DashboardAuth 'DASHBOARD_USER
             :> Post '[JSON] DReg.LogoutRes
       )

handler :: FlowServer API
handler =
  login
    :<|> logout
    :<|> logoutAllMerchants

login :: DReg.LoginReq -> FlowHandler DReg.LoginRes
login = withFlowHandlerAPI . DReg.login

logout :: TokenInfo -> FlowHandler DReg.LogoutRes
logout = withFlowHandlerAPI . DReg.logout

logoutAllMerchants :: TokenInfo -> FlowHandler DReg.LogoutRes
logoutAllMerchants = withFlowHandlerAPI . DReg.logoutAllMerchants
