{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.College where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CollegeE e = College
  { collegeAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    collegeName :: Kernel.Prelude.Text,
    contactName :: Kernel.Prelude.Text,
    contactPhoneNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    contactRole :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.College.College,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type College = CollegeE ('AsEncrypted)

type DecryptedCollege = CollegeE ('AsUnencrypted)

instance EncryptedItem College where
  type Unencrypted College = (DecryptedCollege, HashSalt)
  encryptItem (entity, salt) = do
    contactPhoneNumber_ <- encryptItem $ (,salt) <$> contactPhoneNumber entity
    pure
      College
        { collegeAddress = collegeAddress entity,
          collegeName = collegeName entity,
          contactName = contactName entity,
          contactPhoneNumber = contactPhoneNumber_,
          contactRole = contactRole entity,
          createdAt = createdAt entity,
          id = id entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    contactPhoneNumber_ <- fmap fst <$> decryptItem (contactPhoneNumber entity)
    pure
      ( College
          { collegeAddress = collegeAddress entity,
            collegeName = collegeName entity,
            contactName = contactName entity,
            contactPhoneNumber = contactPhoneNumber_,
            contactRole = contactRole entity,
            createdAt = createdAt entity,
            id = id entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' College where
  type UnencryptedItem College = DecryptedCollege
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
