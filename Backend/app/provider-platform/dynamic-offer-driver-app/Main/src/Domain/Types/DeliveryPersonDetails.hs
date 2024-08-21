{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.DeliveryPersonDetails where

import Domain.Types.RiderDetails
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import qualified Text.Show

data DeliveryPersonDetailsE e = DeliveryPersonDetails
  { id :: Id RiderDetails,
    name :: Text,
    phone :: EncryptedHashedField e Text
  }

instance Show DeliveryPersonDetails where
  show (DeliveryPersonDetails {..}) =
    "DeliveryPersonDetails { id = " <> show id.getId <> ", name = " <> show name <> ", phone = xxxx }"

type DeliveryPersonDetails = DeliveryPersonDetailsE 'AsEncrypted

type DecryptedDeliveryPersonDetails = DeliveryPersonDetailsE 'AsUnencrypted

instance EncryptedItem DeliveryPersonDetails where
  type Unencrypted DeliveryPersonDetails = (DecryptedDeliveryPersonDetails, HashSalt)
  encryptItem (entity, salt) = do
    phone_ <- encryptItem $ (phone entity, salt)
    return
      entity {phone = phone_}
  decryptItem entity = do
    phone_ <- fst <$> decryptItem (phone entity)
    return (entity {phone = phone_}, "")

instance EncryptedItem' DeliveryPersonDetails where
  type UnencryptedItem DeliveryPersonDetails = DecryptedDeliveryPersonDetails
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
