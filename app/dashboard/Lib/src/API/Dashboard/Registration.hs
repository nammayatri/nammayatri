module API.Dashboard.Registration where

import Beckn.Prelude
import Beckn.Utils.Common
import qualified Domain.Action.Dashboard.Registration as DReg
import Environment
import Servant
import Tools.Auth

type API =
  "login"
    :> ReqBody '[JSON] DReg.LoginReq
    :> Post '[JSON] DReg.LoginRes
    :<|> "logout"
      :> DashboardAuth 'DASHBOARD_USER
      :> Post '[JSON] DReg.LogoutRes

handler :: FlowServer API
handler =
  login
    :<|> logout

login :: DReg.LoginReq -> FlowHandler DReg.LoginRes
login = withFlowHandlerAPI . DReg.login

logout :: TokenInfo -> FlowHandler DReg.LogoutRes
logout = withFlowHandlerAPI . DReg.logout
