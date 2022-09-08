module API.Dashboard.Person where

import Beckn.External.Encryption (decrypt)
import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common (withFlowHandlerAPI)
import Domain.Types.Person as DP
import Environment
import Servant hiding (Unauthorized, throwError)
import qualified Storage.Queries.Person as QP
import Tools.Auth
import Tools.Roles.Instances

type API =
  "person"
    :> "list"
    :> TokenAuth (DashboardAccessLevel 'DASHBOARD_ADMIN)
    :> QueryParam "searchString" Text
    :> QueryParam "limit" Integer
    :> QueryParam "offset" Integer
    :> Get '[JSON] ListPersonRes

newtype ListPersonRes = ListPersonRes
  {list :: [PersonEntityRes]}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PersonEntityRes = PersonEntityRes
  { id :: Id DP.Person,
    firstName :: Maybe Text,
    lastName :: Maybe Text,
    role :: Role,
    email :: Text,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    registeredAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

handler :: FlowServer API
handler = listPerson

listPerson :: Id DP.Person -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler ListPersonRes
listPerson _ mbSearchString mbLimit mbOffset = withFlowHandlerAPI do
  personList <- QP.findAllWithLimitOffset mbSearchString mbLimit mbOffset
  respPersonList <- traverse buildPersonEntityRes personList
  return $ ListPersonRes respPersonList

buildPersonEntityRes :: (EsqDBFlow m r, EncFlow m r) => DP.Person -> m PersonEntityRes
buildPersonEntityRes person = do
  decEmail <- decrypt person.email
  decMobNum <- mapM decrypt person.mobileNumber
  return $
    PersonEntityRes
      { id = person.id,
        firstName = person.firstName,
        lastName = person.lastName,
        role = person.role,
        email = decEmail,
        mobileNumber = decMobNum,
        mobileCountryCode = person.mobileCountryCode,
        registeredAt = person.createdAt
      }
