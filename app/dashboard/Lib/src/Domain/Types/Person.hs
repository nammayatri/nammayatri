{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Person where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Types.Id

-- TODO move roles to DB
data Role
  = CUSTOMER
  | DRIVER
  | JUSPAY_OPS
  | JUSPAY_ADMIN
  | CUSTOMER_SERVICE
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, Enum, Bounded, ToSchema)

data PersonE e = Person
  { id :: Id Person,
    firstName :: Maybe Text,
    lastName :: Maybe Text,
    role :: Role,
    email :: EncryptedHashedField e Text,
    mobileNumber :: Maybe (EncryptedHashedField e Text),
    mobileCountryCode :: Maybe Text,
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
    mobileNumber_ <- encryptItem $ (,salt) <$> mobileNumber
    email_ <- encryptItem (email, salt)
    return Person {mobileNumber = mobileNumber_, email = email_, ..}
  decryptItem Person {..} = do
    mobileNumber_ <- fmap fst <$> decryptItem mobileNumber
    email_ <- fst <$> decryptItem email
    return (Person {mobileNumber = mobileNumber_, email = email_, ..}, "")

-- do we need it?
-- instance EncryptedItem' Person where
--   type UnencryptedItem Person = DecryptedPerson
--   toUnencrypted a salt = (a, salt)
--   fromUnencrypted a = fst a
