module Product.Person where

import App.Types
import qualified Beckn.Types.APISuccess as APISuccess
import EulerHS.Prelude
import qualified Storage.Queries.Person as QPerson
import qualified Types.API.Person as Person
import qualified Types.Storage.Person as Person
import Utils.Common (withFlowHandlerAPI)

getPersonDetails :: Person.Person -> FlowHandler Person.GetPersonDetailsRes
getPersonDetails auth =
  pure $
    Person.GetPersonDetailsRes
      { id = auth.id,
        firstName = auth.firstName,
        middleName = auth.middleName,
        lastName = auth.lastName,
        fullName = auth.fullName,
        role = auth.role,
        gender = auth.gender,
        email = auth.email
      }

updatePerson :: Person.Person -> Person.UpdateReq -> FlowHandler APISuccess.APISuccess
updatePerson auth req = withFlowHandlerAPI $ do
  QPerson.updatePersonalInfo
    (auth.id)
    (req.firstName)
    (req.middleName)
    (req.lastName)
    (req.fullName)
    (req.gender)
    (req.email)
    (req.deviceToken)
  pure APISuccess.Success
