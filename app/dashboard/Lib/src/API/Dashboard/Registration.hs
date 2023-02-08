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
