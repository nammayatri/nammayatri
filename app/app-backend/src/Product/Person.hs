module Product.Person where

import App.Types
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import EulerHS.Prelude
import qualified Storage.Queries.Person as QPerson
import qualified Types.API.Person as Person
import Types.Error
import qualified Types.Storage.Person as Person
import Utils.Common (fromMaybeM, withFlowHandlerAPI)

getPersonDetails :: Id Person.Person -> FlowHandler Person.GetPersonDetailsRes
getPersonDetails personId = withFlowHandlerAPI $ do
  person <- QPerson.findById personId >>= fromMaybeM PersonNotFound
  pure $
    Person.GetPersonDetailsRes
      { id = person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        fullName = person.fullName,
        role = person.role,
        gender = person.gender,
        email = person.email
      }

updatePerson :: Id Person.Person -> Person.UpdateReq -> FlowHandler APISuccess.APISuccess
updatePerson personId req = withFlowHandlerAPI $ do
  QPerson.updatePersonalInfo
    personId
    (req.firstName)
    (req.middleName)
    (req.lastName)
    (req.fullName)
    (req.gender)
    (req.email)
    (req.deviceToken)
  pure APISuccess.Success
