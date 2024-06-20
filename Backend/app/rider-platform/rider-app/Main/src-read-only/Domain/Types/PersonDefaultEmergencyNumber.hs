{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PersonDefaultEmergencyNumber where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PersonDefaultEmergencyNumberE e = PersonDefaultEmergencyNumber
  { personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    name :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    contactPersonId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    enableForFollowing :: Kernel.Prelude.Bool,
    enableForShareRide :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    priority :: Kernel.Prelude.Int
  }
  deriving (Generic)

type PersonDefaultEmergencyNumber = PersonDefaultEmergencyNumberE 'AsEncrypted

type DecryptedPersonDefaultEmergencyNumber = PersonDefaultEmergencyNumberE 'AsUnencrypted

instance EncryptedItem PersonDefaultEmergencyNumber where
  type Unencrypted PersonDefaultEmergencyNumber = (DecryptedPersonDefaultEmergencyNumber, HashSalt)
  encryptItem (entity, salt) = do
    mobileNumber_ <- encryptItem (mobileNumber entity, salt)
    pure
      PersonDefaultEmergencyNumber
        { personId = personId entity,
          name = name entity,
          mobileNumber = mobileNumber_,
          mobileCountryCode = mobileCountryCode entity,
          createdAt = createdAt entity,
          contactPersonId = contactPersonId entity,
          enableForFollowing = enableForFollowing entity,
          enableForShareRide = enableForShareRide entity,
          merchantId = merchantId entity,
          priority = priority entity
        }
  decryptItem entity = do
    mobileNumber_ <- fst <$> decryptItem (mobileNumber entity)
    pure
      ( PersonDefaultEmergencyNumber
          { personId = personId entity,
            name = name entity,
            mobileNumber = mobileNumber_,
            mobileCountryCode = mobileCountryCode entity,
            createdAt = createdAt entity,
            contactPersonId = contactPersonId entity,
            enableForFollowing = enableForFollowing entity,
            enableForShareRide = enableForShareRide entity,
            merchantId = merchantId entity,
            priority = priority entity
          },
        ""
      )

instance EncryptedItem' PersonDefaultEmergencyNumber where
  type UnencryptedItem PersonDefaultEmergencyNumber = DecryptedPersonDefaultEmergencyNumber
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
