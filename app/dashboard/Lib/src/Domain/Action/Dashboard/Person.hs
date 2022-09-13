module Domain.Action.Dashboard.Person where

import Beckn.External.Encryption (decrypt)
import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Person as DP
import qualified Storage.Queries.Person as QP

newtype ListPersonRes = ListPersonRes
  {list :: [PersonEntityRes]}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PersonEntityRes = PersonEntityRes
  { id :: Id DP.Person,
    firstName :: Text,
    lastName :: Text,
    role :: Role,
    email :: Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    registeredAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

listPerson ::
  (EsqDBFlow m r, EncFlow m r) =>
  Id DP.Person ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  m ListPersonRes
listPerson _ mbSearchString mbLimit mbOffset = do
  personList <- QP.findAllWithLimitOffset mbSearchString mbLimit mbOffset
  respPersonList <- traverse buildPersonEntityRes personList
  return $ ListPersonRes respPersonList

buildPersonEntityRes :: (EsqDBFlow m r, EncFlow m r) => DP.Person -> m PersonEntityRes
buildPersonEntityRes person = do
  decEmail <- decrypt person.email
  decMobNum <- decrypt person.mobileNumber
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
