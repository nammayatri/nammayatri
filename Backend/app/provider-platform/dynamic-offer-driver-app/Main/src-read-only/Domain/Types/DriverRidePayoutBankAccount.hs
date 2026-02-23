{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverRidePayoutBankAccount where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverRidePayoutBankAccountE e = DriverRidePayoutBankAccount
  { bankAccountNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    bankIfscCode :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.DriverRidePayoutBankAccount.DriverRidePayoutBankAccount,
    rcId :: Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type DriverRidePayoutBankAccount = DriverRidePayoutBankAccountE ('AsEncrypted)

type DecryptedDriverRidePayoutBankAccount = DriverRidePayoutBankAccountE ('AsUnencrypted)

instance EncryptedItem DriverRidePayoutBankAccount where
  type Unencrypted DriverRidePayoutBankAccount = (DecryptedDriverRidePayoutBankAccount, HashSalt)
  encryptItem (entity, salt) = do
    bankAccountNumber_ <- encryptItem $ (,salt) <$> bankAccountNumber entity
    bankIfscCode_ <- encryptItem $ (,salt) <$> bankIfscCode entity
    pure
      DriverRidePayoutBankAccount
        { bankAccountNumber = bankAccountNumber_,
          bankIfscCode = bankIfscCode_,
          driverId = driverId entity,
          id = id entity,
          rcId = rcId entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    bankAccountNumber_ <- fmap fst <$> decryptItem (bankAccountNumber entity)
    bankIfscCode_ <- fmap fst <$> decryptItem (bankIfscCode entity)
    pure
      ( DriverRidePayoutBankAccount
          { bankAccountNumber = bankAccountNumber_,
            bankIfscCode = bankIfscCode_,
            driverId = driverId entity,
            id = id entity,
            rcId = rcId entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' DriverRidePayoutBankAccount where
  type UnencryptedItem DriverRidePayoutBankAccount = DecryptedDriverRidePayoutBankAccount
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
