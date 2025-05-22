{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Fleet.Registration
  ( handler,
    API,
    FleetEndpoint (..),
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
           :<|> FleetOwnerRegisterAPI
       )

type FleetOwnerVerifyAPI =
  "verify"
    :> "otp"
    :> ReqBody '[JSON] DFleet.FleetOwnerLoginReq
    :> Post '[JSON] APISuccess

type FleetOwnerLoginAPI =
  "login"
    :> "otp"
    :> QueryParam "enabled" Bool
    :> ReqBody '[JSON] DFleet.FleetOwnerLoginReq
    :> Post '[JSON] DFleet.FleetOwnerRegisterRes

type FleetOwnerRegisterAPI =
  "register"
    :> QueryParam "requestorId" Text
    :> ReqBody '[JSON] DFleet.FleetOwnerRegisterReq
    :> Post '[JSON] APISuccess

data FleetEndpoint = FleetOwnerRegisterEndpoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord, ToSchema)

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler _ _ =
  fleetOwnerLogin
    :<|> fleetOwnerVerify
    :<|> fleetOwnerRegister

fleetOwnerLogin :: Maybe Bool -> DFleet.FleetOwnerLoginReq -> FlowHandler DFleet.FleetOwnerRegisterRes
fleetOwnerLogin enabled req = withDashboardFlowHandlerAPI $ DFleet.fleetOwnerLogin Nothing enabled req

fleetOwnerVerify :: DFleet.FleetOwnerLoginReq -> FlowHandler APISuccess
fleetOwnerVerify = withDashboardFlowHandlerAPI . DFleet.fleetOwnerVerify

fleetOwnerRegister :: Maybe Text -> DFleet.FleetOwnerRegisterReq -> FlowHandler APISuccess
fleetOwnerRegister mbRequestorId = withDashboardFlowHandlerAPI . DFleet.fleetOwnerRegister mbRequestorId
