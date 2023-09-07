{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Person.Type where

import qualified Domain.Types.Role as DRole
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id

data PersonE e = Person
  { id :: Id Person,
    firstName :: Text,
    lastName :: Text,
    roleId :: Id DRole.Role,
    email :: Maybe (EncryptedHashedField e Text),
    mobileNumber :: EncryptedHashedField e Text,
    mobileCountryCode :: Text,
    passwordHash :: Maybe DbHash,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type Person = PersonE 'AsEncrypted

type DecryptedPerson = PersonE 'AsUnencrypted

instance EncryptedItem Person where
  type Unencrypted Person = (DecryptedPerson, HashSalt)
  encryptItem (Person {..}, salt) = do
    mobileNumber_ <- encryptItem (mobileNumber, salt)
    email_ <- encryptItem $ (,salt) <$> email
    return Person {mobileNumber = mobileNumber_, email = email_, ..}
  decryptItem Person {..} = do
    mobileNumber_ <- fst <$> decryptItem mobileNumber
    email_ <- fmap fst <$> decryptItem email
    return (Person {mobileNumber = mobileNumber_, email = email_, ..}, "")

instance EncryptedItem' Person where
  type UnencryptedItem Person = DecryptedPerson
  toUnencrypted a salt = (a, salt)
  fromUnencrypted a = fst a
