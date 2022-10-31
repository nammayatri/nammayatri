module Domain.Types.Person.API where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.Person.Type
import qualified Domain.Types.Role as DRole

data PersonAPIEntity = PersonAPIEntity
  { id :: Id Person,
    firstName :: Text,
    lastName :: Text,
    role :: DRole.RoleAPIEntity,
    email :: Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    availableMerchants :: [ShortId DMerchant.Merchant],
    registeredAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

makePersonAPIEntity :: DecryptedPerson -> DRole.Role -> [ShortId DMerchant.Merchant] -> PersonAPIEntity
makePersonAPIEntity Person {..} personRole availableMerchants =
  PersonAPIEntity
    { registeredAt = createdAt,
      role = DRole.mkRoleAPIEntity personRole,
      ..
    }
