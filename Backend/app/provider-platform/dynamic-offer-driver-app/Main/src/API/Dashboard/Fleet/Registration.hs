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
    handler,
    API,
  )
where

import qualified Domain.Action.Dashboard.Fleet.Registration as DFleet
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant hiding (throwError)

type API =
  "fleet"
    :> ( FleetOwnerLoginAPI
           :<|> FleetOwnerVerifyAPI
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

handler :: FlowServer API
handler =
  fleetOwnerLogin
    :<|> fleetOwnerVerify

fleetOwnerLogin :: DFleet.FleetOwnerLoginReq -> FlowHandler APISuccess
fleetOwnerLogin = withFlowHandlerAPI . DFleet.fleetOwnerLogin

fleetOwnerVerify :: DFleet.FleetOwnerLoginReq -> FlowHandler APISuccess
fleetOwnerVerify = withFlowHandlerAPI . DFleet.fleetOwnerVerify
