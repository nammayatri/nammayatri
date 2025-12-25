{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Fleet.Registration
  ( FleetOwnerLoginAPI,
    fleetOwnerLogin,
    fleetOwnerVerify,
    FleetOwnerRegisterAPI,
    handler,
    API,
  )
where

import qualified Domain.Action.Dashboard.Fleet.Registration as DFleet
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()

type API =
  "fleet"
    :> ( FleetOwnerLoginAPI
           :<|> FleetOwnerVerifyAPI
           :<|> FleetOwnerRegisterHelperAPI
       )

type FleetOwnerVerifyAPI =
  "verify"
    :> "otp"
    :> ReqBody '[JSON] DFleet.FleetOwnerLoginReq
    :> Post '[JSON] APISuccess

type FleetOwnerLoginAPI =
  "login"
    :> "otp"
    :> ReqBody '[JSON] DFleet.FleetOwnerLoginReq
    :> Post '[JSON] APISuccess

type FleetOwnerRegisterAPI =
  "register"
    :> ReqBody '[JSON] DFleet.FleetOwnerRegisterReq
    :> Post '[JSON] DFleet.FleetOwnerRegisterRes

type FleetOwnerRegisterHelperAPI =
  ( "register"
      :> QueryParam "enabled" Bool
      :> ReqBody '[JSON] DFleet.FleetOwnerRegisterReq
      :> Post '[JSON] DFleet.FleetOwnerRegisterRes
  )

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler _ _ =
  fleetOwnerLogin
    :<|> fleetOwnerVerify
    :<|> fleetOwnerRegister

fleetOwnerLogin :: DFleet.FleetOwnerLoginReq -> FlowHandler APISuccess
fleetOwnerLogin = withDashboardFlowHandlerAPI . DFleet.fleetOwnerLogin

fleetOwnerVerify :: DFleet.FleetOwnerLoginReq -> FlowHandler APISuccess
fleetOwnerVerify = withDashboardFlowHandlerAPI . DFleet.fleetOwnerVerify

fleetOwnerRegister :: Maybe Bool -> DFleet.FleetOwnerRegisterReq -> FlowHandler DFleet.FleetOwnerRegisterRes
fleetOwnerRegister enabled req = withDashboardFlowHandlerAPI $ DFleet.fleetOwnerRegister req enabled
