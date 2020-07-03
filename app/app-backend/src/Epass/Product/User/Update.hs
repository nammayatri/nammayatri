{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Epass.Product.User.Update where

import Beckn.Types.App (PersonId (..))
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common
import Data.Aeson
import qualified Data.List as List
import Data.Time
import Epass.Product.Common
import qualified Epass.Storage.Queries.Location as Location
import qualified Epass.Storage.Queries.Organization as Org
import Epass.Types.API.Common
import Epass.Types.API.User
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.Location as Location
import qualified Epass.Types.Storage.Organization as Org
import Epass.Utils.Common
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegToken

update :: RegToken -> PersonId -> UpdateReq -> FlowHandler UpdateRes
update regToken userId req@UpdateReq {..} = withFlowHandler $ do
  verifyToken regToken
  person <-
    Person.findById userId
      >>= fromMaybeM500 "Couldnot find user"
  let role = person ^. #_role
  when
    (isJust _status && (role == Person.DRIVER || role == Person.USER))
    (L.throwException $ err500 {errBody = "UNAUTHORIZED"})
  let updatedPerson =
        person
          { Person._firstName = _middleName <|> (person ^. #_firstName),
            Person._middleName = _middleName <|> (person ^. #_middleName),
            Person._lastName = _lastName <|> (person ^. #_lastName),
            Person._fullName = _fullName <|> (person ^. #_fullName),
            Person._gender = fromMaybe (person ^. #_gender) _gender,
            Person._email = _email <|> (person ^. #_email),
            Person._organizationId = _organizationId <|> (person ^. #_organizationId),
            Person._locationId = _locationId <|> (person ^. #_locationId),
            Person._description = _description <|> (person ^. #_description),
            Person._status = fromMaybe (person ^. #_status) _status
          }
  Person.updateMultiple userId updatedPerson
  UpdateRes
    <$> ( Person.findById userId
            >>= fromMaybeM500 "Couldnot find user"
        )

delete :: RegToken -> PersonId -> FlowHandler Ack
delete regToken userId = withFlowHandler $ do
  verifyToken regToken
  Person.deleteById userId
  RegToken.deleteByPersonId (_getPersonId userId)
  sendAck
