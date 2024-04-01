{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PersonDefaultEmergencyNumber where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PersonDefaultEmergencyNumberE e = PersonDefaultEmergencyNumber
  { merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    name :: Kernel.Prelude.Text,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
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
        { merchantId = merchantId entity,
          mobileCountryCode = mobileCountryCode entity,
          mobileNumber = mobileNumber_,
          name = name entity,
          personId = personId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    mobileNumber_ <- fst <$> decryptItem (mobileNumber entity)
    pure
      ( PersonDefaultEmergencyNumber
          { merchantId = merchantId entity,
            mobileCountryCode = mobileCountryCode entity,
            mobileNumber = mobileNumber_,
            name = name entity,
            personId = personId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' PersonDefaultEmergencyNumber where
  type UnencryptedItem PersonDefaultEmergencyNumber = DecryptedPersonDefaultEmergencyNumber
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
