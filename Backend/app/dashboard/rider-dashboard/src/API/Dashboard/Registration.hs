{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Registration where

import "lib-dashboard" API.Dashboard.Registration (API)
import qualified Domain.Action.Dashboard.Registration as DReg
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth

-- Note : Type of API is defined in lib-dashboard/API/Dashboard/Registration.hs

handler :: FlowServer API
handler =
  login
    :<|> logout
    :<|> logoutAllMerchants
    :<|> enable2fa
    :<|> switchMerchant
    :<|> switchMerchantAndCity
    :<|> registerFleetOwner

login :: DReg.LoginReq -> FlowHandler DReg.LoginRes
login = withFlowHandlerAPI' . DReg.login

logout :: TokenInfo -> FlowHandler DReg.LogoutRes
logout = withFlowHandlerAPI' . DReg.logout

logoutAllMerchants :: TokenInfo -> FlowHandler DReg.LogoutRes
logoutAllMerchants = withFlowHandlerAPI' . DReg.logoutAllMerchants

enable2fa :: DReg.Enable2FAReq -> FlowHandler DReg.Enable2FARes
enable2fa = withFlowHandlerAPI' . DReg.enable2fa

switchMerchant :: TokenInfo -> DReg.SwitchMerchantReq -> FlowHandler DReg.LoginRes
switchMerchant token = withFlowHandlerAPI' . DReg.switchMerchant token

switchMerchantAndCity :: TokenInfo -> DReg.SwitchMerchantAndCityReq -> FlowHandler DReg.LoginRes
switchMerchantAndCity token = withFlowHandlerAPI' . DReg.switchMerchantAndCity token

registerFleetOwner :: DReg.FleetRegisterReq -> FlowHandler APISuccess
registerFleetOwner = withFlowHandlerAPI' . DReg.registerFleetOwner
