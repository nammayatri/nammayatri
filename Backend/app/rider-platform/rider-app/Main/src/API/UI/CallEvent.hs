{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.CallEvent
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.CallEvent as DCE
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "callEvent"
    :> TokenAuth
    :> ReqBody '[JSON] DCE.CallEventReq
    :> Post '[JSON] APISuccess.APISuccess

handler :: FlowServer API
handler = logCallEvent

logCallEvent :: (Id Person.Person, Id Merchant.Merchant) -> DCE.CallEventReq -> FlowHandler APISuccess.APISuccess
logCallEvent (_, _) = withFlowHandlerAPI . DCE.logCallEvent
