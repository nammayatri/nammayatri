module Product.Person where

import Beckn.TypeClass.Transform
import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Types.API.Person

updatePerson :: Text -> Maybe Text -> UpdatePersonReq -> FlowHandler UpdatePersonRes
updatePerson personId token req = withFlowHandler $ do
  QR.verifyAuth token
  person <- QP.findPersonById (PersonId personId)
  updatedPerson <- transformFlow2 req person
  QP.updatePersonRec (PersonId personId) updatedPerson
  return $ UpdatePersonRes updatedPerson
