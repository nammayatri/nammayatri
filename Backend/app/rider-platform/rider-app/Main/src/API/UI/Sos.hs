{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Sos
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.Sos as DSos
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Sos as Sos
import Environment
import EulerHS.Prelude
import Kernel.ServantMultipart
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "sos"
    :> Capture "sosId" (Id Sos.Sos)
    :> "addVideo"
    :> TokenAuth
    :> MultipartForm Tmp DSos.SOSVideoUploadReq
    :> Post '[JSON] DSos.AddSosVideoRes

handler :: FlowServer API
handler = addSosVideo

addSosVideo :: Id Sos.Sos -> (Id Person.Person, Id Merchant.Merchant) -> DSos.SOSVideoUploadReq -> FlowHandler DSos.AddSosVideoRes
addSosVideo sosId (personId, _) = withFlowHandlerAPI . withPersonIdLogTag personId . DSos.addSosVideo sosId personId
