{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.RiderDetails where

import Data.Time
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import Kernel.Types.Id

data RiderDetailsE e = RiderDetails
  { id :: Id RiderDetails,
    mobileCountryCode :: Text,
    mobileNumber :: EncryptedHashedField e Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type RiderDetails = RiderDetailsE 'AsEncrypted

type RiderDetailsDecrypted = RiderDetailsE 'AsUnencrypted

instance EncryptedItem RiderDetails where
  type Unencrypted RiderDetails = (RiderDetailsDecrypted, HashSalt)
  encryptItem (RiderDetails {..}, salt) = do
    mobileNumber_ <- encryptItem (mobileNumber, salt)
    return RiderDetails {mobileNumber = mobileNumber_, ..}
  decryptItem RiderDetails {..} = do
    mobileNumber_ <- fst <$> decryptItem mobileNumber
    return (RiderDetails {mobileNumber = mobileNumber_, ..}, "")

instance EncryptedItem' RiderDetails where
  type UnencryptedItem RiderDetails = RiderDetailsDecrypted
  toUnencrypted a salt = (a, salt)
  fromUnencrypted a = fst a
