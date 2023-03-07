{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Profile
  ( DProfile.ProfileRes,
    DProfile.UpdateProfileReq (..),
    DProfile.UpdateProfileResp,
    DProfile.UpdateProfileDefaultEmergencyNumbersReq (..),
    DProfile.PersonDefaultEmergencyNumber (..),
    DProfile.UpdateProfileDefaultEmergencyNumbersResp,
    DProfile.GetProfileDefaultEmergencyNumbersResp (..),
    API,
    handler,
  )
where

import qualified Domain.Action.UI.Profile as DProfile
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "profile"
    :> ( TokenAuth
           :> Get '[JSON] DProfile.ProfileRes
           :<|> TokenAuth
             :> ReqBody '[JSON] DProfile.UpdateProfileReq
             :> Post '[JSON] APISuccess.APISuccess
           :<|> "defaultEmergencyNumbers"
             :> ( TokenAuth
                    :> ReqBody '[JSON] DProfile.UpdateProfileDefaultEmergencyNumbersReq
                    :> Post '[JSON] DProfile.UpdateProfileDefaultEmergencyNumbersResp
                      :<|> TokenAuth
                    :> Get '[JSON] DProfile.GetProfileDefaultEmergencyNumbersResp
                )
       )

handler :: FlowServer API
handler =
  getPersonDetails
    :<|> updatePerson
    :<|> updateDefaultEmergencyNumbers
    :<|> getDefaultEmergencyNumbers

getPersonDetails :: Id Person.Person -> FlowHandler DProfile.ProfileRes
getPersonDetails = withFlowHandlerAPI . DProfile.getPersonDetails

updatePerson :: Id Person.Person -> DProfile.UpdateProfileReq -> FlowHandler APISuccess.APISuccess
updatePerson personId = withFlowHandlerAPI . withPersonIdLogTag personId . DProfile.updatePerson personId

updateDefaultEmergencyNumbers :: Id Person.Person -> DProfile.UpdateProfileDefaultEmergencyNumbersReq -> FlowHandler DProfile.UpdateProfileDefaultEmergencyNumbersResp
updateDefaultEmergencyNumbers personId = withFlowHandlerAPI . withPersonIdLogTag personId . DProfile.updateDefaultEmergencyNumbers personId

getDefaultEmergencyNumbers :: Id Person.Person -> FlowHandler DProfile.GetProfileDefaultEmergencyNumbersResp
getDefaultEmergencyNumbers = withFlowHandlerAPI . DProfile.getDefaultEmergencyNumbers
