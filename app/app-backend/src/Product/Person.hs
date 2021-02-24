{-# LANGUAGE OverloadedLabels #-}

module Product.Person where

import App.Types (FlowHandler)
import qualified Beckn.Types.APIResult as APIResult
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common (withFlowHandler)
import EulerHS.Prelude
import qualified Storage.Queries.Person as QPerson
import qualified Types.API.Person as Person

getPersonDetails :: Person.Person -> FlowHandler Person.GetPersonDetailsRes
getPersonDetails auth =
  pure $
    Person.GetPersonDetailsRes
      { firstName = auth ^. #_firstName,
        middleName = auth ^. #_middleName,
        lastName = auth ^. #_lastName,
        fullName = auth ^. #_fullName,
        role = auth ^. #_role,
        gender = auth ^. #_gender
      }

updatePerson :: Person.Person -> Person.UpdateReq -> FlowHandler APIResult.APIResult
updatePerson auth req = withFlowHandler $ do
  QPerson.updatePersonalInfo
    (auth ^. #_id)
    (req ^. #firstName)
    (req ^. #middleName)
    (req ^. #lastName)
    (req ^. #fullName)
    (req ^. #gender)
    (req ^. #deviceToken)
  pure APIResult.Success
