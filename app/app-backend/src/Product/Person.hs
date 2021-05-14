{-# LANGUAGE OverloadedLabels #-}

module Product.Person where

import App.Types (FlowHandler)
import qualified Beckn.Types.APISuccess as APISuccess
import qualified Beckn.Types.Storage.Person as Person
import EulerHS.Prelude
import qualified Storage.Queries.Person as QPerson
import qualified Types.API.Person as Person
import Utils.Common (withFlowHandlerAPI)

getPersonDetails :: Person.Person -> FlowHandler Person.GetPersonDetailsRes
getPersonDetails auth =
  pure $
    Person.GetPersonDetailsRes
      { firstName = auth ^. #_firstName,
        middleName = auth ^. #_middleName,
        lastName = auth ^. #_lastName,
        fullName = auth ^. #_fullName,
        role = auth ^. #_role,
        gender = auth ^. #_gender,
        email = auth ^. #_email
      }

updatePerson :: Person.Person -> Person.UpdateReq -> FlowHandler APISuccess.APISuccess
updatePerson auth req = withFlowHandlerAPI $ do
  QPerson.updatePersonalInfo
    (auth ^. #_id)
    (req ^. #firstName)
    (req ^. #middleName)
    (req ^. #lastName)
    (req ^. #fullName)
    (req ^. #gender)
    (req ^. #email)
    (req ^. #deviceToken)
  pure APISuccess.Success
