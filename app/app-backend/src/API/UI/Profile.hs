module API.UI.Profile
  ( DProfile.ProfileRes,
    DProfile.UpdateProfileReq (..),
    API,
    handler,
  )
where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.UI.Profile as DProfile
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import Servant
import Utils.Auth

type API =
  "profile"
    :> ( TokenAuth
           :> Get '[JSON] DProfile.ProfileRes
           :<|> TokenAuth
             :> ReqBody '[JSON] DProfile.UpdateProfileReq
             :> Post '[JSON] APISuccess.APISuccess
       )

handler :: FlowServer API
handler =
  getPersonDetails
    :<|> updatePerson

getPersonDetails :: Id Person.Person -> FlowHandler DProfile.ProfileRes
getPersonDetails = withFlowHandlerAPI . DProfile.getPersonDetails

updatePerson :: Id Person.Person -> DProfile.UpdateProfileReq -> FlowHandler APISuccess.APISuccess
updatePerson personId = withFlowHandlerAPI . withPersonIdLogTag personId . DProfile.updatePerson personId
