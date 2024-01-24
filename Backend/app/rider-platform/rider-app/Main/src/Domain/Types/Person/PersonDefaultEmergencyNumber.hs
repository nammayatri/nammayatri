{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Person.PersonDefaultEmergencyNumber where

import Domain.Types.Person (Person)
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id

data PersonDefaultEmergencyNumberE e = PersonDefaultEmergencyNumber
  { personId :: Id Person,
    name :: Text,
    mobileNumber :: EncryptedHashedField e Text,
    mobileCountryCode :: Text,
    createdAt :: UTCTime,
    contactPersonId :: Maybe (Id Person),
    enableForFollowing :: Bool,
    priority :: Int
  }
  deriving (Generic)

type PersonDefaultEmergencyNumber = PersonDefaultEmergencyNumberE 'AsEncrypted

type DecryptedPersonDefaultEmergencyNumber = PersonDefaultEmergencyNumberE 'AsUnencrypted

instance EncryptedItem PersonDefaultEmergencyNumber where
  type Unencrypted PersonDefaultEmergencyNumber = (DecryptedPersonDefaultEmergencyNumber, HashSalt)
  encryptItem (PersonDefaultEmergencyNumber {..}, salt) = do
    mobileNumber_ <- encryptItem (mobileNumber, salt)
    return PersonDefaultEmergencyNumber {mobileNumber = mobileNumber_, ..}
  decryptItem PersonDefaultEmergencyNumber {..} = do
    mobileNumber_ <- fst <$> decryptItem mobileNumber
    return (PersonDefaultEmergencyNumber {mobileNumber = mobileNumber_, ..}, "")

instance EncryptedItem' PersonDefaultEmergencyNumber where
  type UnencryptedItem PersonDefaultEmergencyNumber = DecryptedPersonDefaultEmergencyNumber
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data PersonDefaultEmergencyNumberAPIEntity = PersonDefaultEmergencyNumberAPIEntity
  { personId :: Id Person,
    name :: Text,
    mobileCountryCode :: Text,
    mobileNumber :: Text,
    priority :: Int,
    contactPersonId :: Maybe (Id Person),
    enableForFollowing :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

makePersonDefaultEmergencyNumberAPIEntity :: DecryptedPersonDefaultEmergencyNumber -> PersonDefaultEmergencyNumberAPIEntity
makePersonDefaultEmergencyNumberAPIEntity PersonDefaultEmergencyNumber {..} =
  PersonDefaultEmergencyNumberAPIEntity
    { ..
    }
