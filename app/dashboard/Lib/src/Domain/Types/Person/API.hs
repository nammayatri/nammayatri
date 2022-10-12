-- {-# LANGUAGE ApplicativeDo #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Person.API where

import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Person.Type
import qualified Domain.Types.Role as DRole
import qualified Domain.Types.ServerName as DSN

data PersonAPIEntity = PersonAPIEntity
  { id :: Id Person,
    firstName :: Text,
    lastName :: Text,
    role :: DRole.RoleAPIEntity,
    email :: Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    availableServers :: [DSN.ServerName],
    registeredAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

makePersonAPIEntity :: DecryptedPerson -> DRole.Role -> [DSN.ServerName] -> PersonAPIEntity
makePersonAPIEntity Person {..} personRole availableServers =
  PersonAPIEntity
    { registeredAt = createdAt,
      role = DRole.mkRoleAPIEntity personRole,
      ..
    }
