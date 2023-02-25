{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.RiderDetails where

import Data.Time
import Domain.Types.Person (Person)
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import Kernel.Types.Id

data RiderDetailsE e = RiderDetails
  { id :: Id RiderDetails,
    mobileCountryCode :: Text,
    mobileNumber :: EncryptedHashedField e Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    referralCode :: Maybe Text,
    referredByDriver :: Maybe (Id Person),
    referredAt :: Maybe UTCTime,
    hasTakenRide :: Bool
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
