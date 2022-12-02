{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.RiderDetails where

import Beckn.External.Encryption
import Beckn.Types.Id
import Data.Time
import EulerHS.Prelude hiding (id)

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
