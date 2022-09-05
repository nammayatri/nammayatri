{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Person where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Types.Id

data Role
  = USER
  | ADMIN
  | JUSPAY_OPS
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, Enum, Bounded, ToSchema)

data PersonE e = Person
  { id :: Id Person,
    firstName :: Maybe Text,
    lastName :: Maybe Text,
    role :: Role,
    email :: Maybe (EncryptedHashedField e Text),
    mobileNumber :: Maybe (EncryptedHashedField e Text),
    mobileCountryCode :: Maybe Text,
    passwordHash :: Maybe DbHash,
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
    mobileNumber_ <- encryptItem $ (,salt) <$> mobileNumber
    email_ <- encryptItem $ (,salt) <$> email
    return Person {mobileNumber = mobileNumber_, email = email_, ..}
  decryptItem Person {..} = do
    mobileNumber_ <- fmap fst <$> decryptItem mobileNumber
    email_ <- fmap fst <$> decryptItem email
    return (Person {mobileNumber = mobileNumber_, email = email_, ..}, "")

-- instance EncryptedItem' Person where
--   type UnencryptedItem Person = DecryptedPerson
--   toUnencrypted a salt = (a, salt)
--   fromUnencrypted a = fst a
