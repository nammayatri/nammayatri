{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Person where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Role as DRole

data PersonE e = Person
  { id :: Id Person,
    firstName :: Text,
    lastName :: Text,
    roleId :: Id DRole.Role,
    email :: EncryptedHashedField e Text,
    mobileNumber :: EncryptedHashedField e Text,
    mobileCountryCode :: Text,
    passwordHash :: DbHash,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type Person = PersonE 'AsEncrypted

type DecryptedPerson = PersonE 'AsUnencrypted

deriving instance Show DecryptedPerson

instance EncryptedItem Person where
  type Unencrypted Person = (DecryptedPerson, HashSalt)
  encryptItem (Person {..}, salt) = do
    mobileNumber_ <- encryptItem (mobileNumber, salt)
    email_ <- encryptItem (email, salt)
    return Person {mobileNumber = mobileNumber_, email = email_, ..}
  decryptItem Person {..} = do
    mobileNumber_ <- fst <$> decryptItem mobileNumber
    email_ <- fst <$> decryptItem email
    return (Person {mobileNumber = mobileNumber_, email = email_, ..}, "")

instance EncryptedItem' Person where
  type UnencryptedItem Person = DecryptedPerson
  toUnencrypted a salt = (a, salt)
  fromUnencrypted a = fst a

data PersonAPIEntity = PersonAPIEntity
  { id :: Id Person,
    firstName :: Text,
    lastName :: Text,
    role :: DRole.RoleAPIEntity,
    email :: Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    registeredAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

makePersonAPIEntity :: DecryptedPerson -> DRole.Role -> PersonAPIEntity
makePersonAPIEntity Person {..} personRole =
  PersonAPIEntity
    { registeredAt = createdAt,
      role = DRole.mkRoleAPIEntity personRole,
      ..
    }
