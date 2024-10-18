{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.RideBooking.Driver where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.DriverRegistration as Common
import qualified Domain.Action.Dashboard.Management.DriverRegistration as DReg
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type AuthAPI =
  Common.AuthAPI
    :<|> VerifyAPI

type VerifyAPI =
  Capture "authId" Text
    :> Capture "mbFleet" Bool
    :> Capture "fleetOwnerId" Text
    :> "verify"
    :> ReqBody '[JSON] Common.AuthVerifyReq
    :> Post '[JSON] APISuccess

authHandler :: ShortId DM.Merchant -> Context.City -> FlowServer AuthAPI
authHandler merchantId city =
  auth merchantId city
    :<|> verify

auth :: ShortId DM.Merchant -> Context.City -> Common.AuthReq -> FlowHandler Common.AuthRes
auth merchantShortId opCity = withDashboardFlowHandlerAPI . DReg.auth merchantShortId opCity

verify :: Text -> Bool -> Text -> Common.AuthVerifyReq -> FlowHandler APISuccess
verify authId mbFleet fleetOwnerId req = withDashboardFlowHandlerAPI $ DReg.verify authId mbFleet fleetOwnerId req
