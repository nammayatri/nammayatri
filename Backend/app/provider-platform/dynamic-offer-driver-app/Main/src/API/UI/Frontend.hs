{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Frontend
  ( DFrontend.GetDriverFlowStatusRes,
    API,
    handler,
  )
where

import qualified Domain.Action.UI.Frontend as DFrontend
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "frontend"
    :> "flowStatus"
    :> TokenAuth
    :> Get '[JSON] DFrontend.GetDriverFlowStatusRes

handler :: FlowServer API
handler =
  getDriverFlowStatus

getDriverFlowStatus :: (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DFrontend.GetDriverFlowStatusRes
getDriverFlowStatus = withFlowHandlerAPI . DFrontend.getDriverFlowStatus
