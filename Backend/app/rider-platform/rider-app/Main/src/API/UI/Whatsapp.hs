{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Whatsapp where

import qualified Domain.Action.UI.Whatsapp as Whatsapp
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment (FlowHandler, FlowServer)
import EulerHS.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Kernel.Utils.Logging
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "whatsapp"
    :> ( "opt"
           :> TokenAuth
           :> ReqBody '[JSON] Whatsapp.OptAPIRequest
           :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler = whatsAppOptAPI

whatsAppOptAPI :: (Id Person.Person, Id Merchant.Merchant) -> Whatsapp.OptAPIRequest -> FlowHandler APISuccess
whatsAppOptAPI (personId, _) = withFlowHandlerAPI . withPersonIdLogTag personId . Whatsapp.whatsAppOptAPI personId
