module Product.Profile where

import App.Types
import Beckn.External.Encryption
import Beckn.Storage.Esqueleto (runTransaction)
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import Beckn.Utils.Logging
import qualified Domain.Types.Person as Person
import EulerHS.Prelude
import qualified Storage.Queries.Person as QPerson
import qualified Types.API.Profile as Profile
import Types.Error
import Utils.Common (fromMaybeM, withFlowHandlerAPI)

getPersonDetails :: Id Person.Person -> FlowHandler Profile.ProfileRes
getPersonDetails personId = withFlowHandlerAPI $ do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  decPerson <- decrypt person
  return $ Person.makePersonAPIEntity decPerson

updatePerson :: Id Person.Person -> Profile.UpdateProfileReq -> FlowHandler APISuccess.APISuccess
updatePerson personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  mbEncEmail <- encrypt `mapM` req.email
  runTransaction $
    QPerson.updatePersonalInfo
      personId
      (req.firstName)
      (req.middleName)
      (req.lastName)
      mbEncEmail
      (req.deviceToken)
  pure APISuccess.Success
