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
import Kernel.Prelude
import Kernel.Types.Flow (FlowR)
import Kernel.Utils.Common
import Servant
import Tools.Auth.Dashboard
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow, withDashboardDbFlowHandlerAPI)

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
           :<|> "enable2Fa"
             :> ReqBody '[JSON] DReg.Enable2FAReq
             :> Post '[JSON] DReg.Enable2FARes
           :<|> "initiate2FaSetup"
             :> ReqBody '[JSON] DReg.Initiate2FASetupReq
             :> Post '[JSON] DReg.Initiate2FASetupRes
           :<|> "verify2FaSetup"
             :> ReqBody '[JSON] DReg.Verify2FASetupReq
             :> Post '[JSON] DReg.Enable2FARes
           :<|> "twoFaStatus"
             :> DashboardAuth 'DASHBOARD_USER
             :> Get '[JSON] DReg.TwoFaStatusRes
           :<|> "twoFaAdminReset"
             :> DashboardAuth 'DASHBOARD_USER
             :> ReqBody '[JSON] DReg.TwoFaAdminResetReq
             :> Post '[JSON] DReg.TwoFaAdminResetRes
           :<|> "twoFaDispatchDeadlineNotifications"
             :> DashboardAuth 'DASHBOARD_USER
             :> Post '[JSON] DReg.DispatchNotificationsRes
           :<|> "switchMerchant"
             :> DashboardAuth 'DASHBOARD_USER
             :> ReqBody '[JSON] DReg.SwitchMerchantReq
             :> Post '[JSON] DReg.LoginRes
           :<|> "switchMerchantAndCity"
             :> DashboardAuth 'DASHBOARD_USER
             :> ReqBody '[JSON] DReg.SwitchMerchantAndCityReq
             :> Post '[JSON] DReg.LoginRes
       )

handler :: DashboardLoginFlow (FlowR r) r => FlowServerR r API
handler =
  login
    :<|> logout
    :<|> logoutAllMerchants
    :<|> enable2fa
    :<|> initiate2FASetup
    :<|> verify2FASetup
    :<|> twoFaStatus
    :<|> twoFaAdminReset
    :<|> twoFaDispatchDeadlineNotifications
    :<|> switchMerchant
    :<|> switchMerchantAndCity

login :: DashboardLoginFlow (FlowR r) r => DReg.LoginReq -> FlowHandlerR r DReg.LoginRes
login = withDashboardDbFlowHandlerAPI . DReg.login

logout :: DashboardLoginFlow (FlowR r) r => TokenInfo -> FlowHandlerR r DReg.LogoutRes
logout = withDashboardDbFlowHandlerAPI . DReg.logout

logoutAllMerchants :: DashboardLoginFlow (FlowR r) r => TokenInfo -> FlowHandlerR r DReg.LogoutRes
logoutAllMerchants = withDashboardDbFlowHandlerAPI . DReg.logoutAllMerchants

enable2fa :: DashboardLoginFlow (FlowR r) r => DReg.Enable2FAReq -> FlowHandlerR r DReg.Enable2FARes
enable2fa = withDashboardDbFlowHandlerAPI . DReg.enable2fa

initiate2FASetup :: DashboardLoginFlow (FlowR r) r => DReg.Initiate2FASetupReq -> FlowHandlerR r DReg.Initiate2FASetupRes
initiate2FASetup = withDashboardDbFlowHandlerAPI . DReg.initiate2FASetup

verify2FASetup :: DashboardLoginFlow (FlowR r) r => DReg.Verify2FASetupReq -> FlowHandlerR r DReg.Enable2FARes
verify2FASetup = withDashboardDbFlowHandlerAPI . DReg.verify2FASetup

twoFaStatus :: DashboardLoginFlow (FlowR r) r => TokenInfo -> FlowHandlerR r DReg.TwoFaStatusRes
twoFaStatus = withDashboardDbFlowHandlerAPI . DReg.getTwoFaStatus

twoFaAdminReset :: DashboardLoginFlow (FlowR r) r => TokenInfo -> DReg.TwoFaAdminResetReq -> FlowHandlerR r DReg.TwoFaAdminResetRes
twoFaAdminReset token = withDashboardDbFlowHandlerAPI . DReg.adminResetTwoFa token

twoFaDispatchDeadlineNotifications :: DashboardLoginFlow (FlowR r) r => TokenInfo -> FlowHandlerR r DReg.DispatchNotificationsRes
twoFaDispatchDeadlineNotifications = withDashboardDbFlowHandlerAPI . DReg.dispatchTwoFaDeadlineNotifications

switchMerchant :: DashboardLoginFlow (FlowR r) r => TokenInfo -> DReg.SwitchMerchantReq -> FlowHandlerR r DReg.LoginRes
switchMerchant token = withDashboardDbFlowHandlerAPI . DReg.switchMerchant token

switchMerchantAndCity :: DashboardLoginFlow (FlowR r) r => TokenInfo -> DReg.SwitchMerchantAndCityReq -> FlowHandlerR r DReg.LoginRes
switchMerchantAndCity token = withDashboardDbFlowHandlerAPI . DReg.switchMerchantAndCity token
