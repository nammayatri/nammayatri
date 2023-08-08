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
    getPersonDetails,
    updatePerson,
    handler,
  )
where

import qualified Domain.Action.UI.Profile as DProfile
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.DisabilityType as DTypes
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
           :<|> "disabilityTypes"
             :> TokenAuth
             :> Get '[JSON] [DTypes.DisabilityType]
       )

handler :: FlowServer API
handler =
  getPersonDetails
    :<|> updatePerson
    :<|> updateDefaultEmergencyNumbers
    :<|> getDefaultEmergencyNumbers
    :<|> getAllDisabilities

getPersonDetails :: (Id Person.Person, Id Merchant.Merchant) -> FlowHandler DProfile.ProfileRes
getPersonDetails = withFlowHandlerAPI . DProfile.getPersonDetails

updatePerson :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.UpdateProfileReq -> FlowHandler APISuccess.APISuccess
updatePerson (personId, _) = withFlowHandlerAPI . withPersonIdLogTag personId . DProfile.updatePerson personId

updateDefaultEmergencyNumbers :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.UpdateProfileDefaultEmergencyNumbersReq -> FlowHandler DProfile.UpdateProfileDefaultEmergencyNumbersResp
updateDefaultEmergencyNumbers (personId, _) = withFlowHandlerAPI . withPersonIdLogTag personId . DProfile.updateDefaultEmergencyNumbers personId

getDefaultEmergencyNumbers :: (Id Person.Person, Id Merchant.Merchant) -> FlowHandler DProfile.GetProfileDefaultEmergencyNumbersResp
getDefaultEmergencyNumbers = withFlowHandlerAPI . DProfile.getDefaultEmergencyNumbers

getAllDisabilities :: (Id Person.Person, Id Merchant.Merchant) -> FlowHandler [DTypes.DisabilityType]
getAllDisabilities = withFlowHandlerAPI . DProfile.getAllDisabilities
