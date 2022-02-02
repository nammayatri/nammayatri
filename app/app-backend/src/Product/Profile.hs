module Product.Profile where

import App.Types
import Beckn.External.Encryption (decrypt)
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import Beckn.Utils.Logging
import EulerHS.Prelude
import qualified Storage.Queries.Person as QPerson
import qualified Types.API.Profile as Profile
import Types.Error
import qualified Types.Storage.Person as Person
import Utils.Common (fromMaybeM, withFlowHandlerAPI)

getPersonDetails :: Id Person.Person -> FlowHandler Profile.ProfileRes
getPersonDetails personId = withFlowHandlerAPI $ do
  person <- QPerson.findById personId >>= fromMaybeM PersonNotFound
  decPerson <- decrypt person
  return $ Person.makePersonAPIEntity decPerson

updatePerson :: Id Person.Person -> Profile.UpdateProfileReq -> FlowHandler APISuccess.APISuccess
updatePerson personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  QPerson.updatePersonalInfo
    personId
    (req.firstName)
    (req.middleName)
    (req.lastName)
    (req.deviceToken)
  pure APISuccess.Success
