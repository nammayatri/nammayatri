{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Permit.Domain.Types.PermitApplicant where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Permit.Domain.Types.Common
import qualified Tools.Beam.UtilsTH

data PermitApplicantE e = PermitApplicant
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplicant.PermitApplicant,
    merchantId :: Kernel.Types.Id.Id Lib.Permit.Domain.Types.Common.Merchant,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type PermitApplicant = PermitApplicantE ('AsEncrypted)

type DecryptedPermitApplicant = PermitApplicantE ('AsUnencrypted)

instance EncryptedItem PermitApplicant where
  type Unencrypted PermitApplicant = (DecryptedPermitApplicant, HashSalt)
  encryptItem (entity, salt) = do
    mobileNumber_ <- encryptItem (mobileNumber entity, salt)
    pure
      PermitApplicant
        { createdAt = createdAt entity,
          id = id entity,
          merchantId = merchantId entity,
          mobileCountryCode = mobileCountryCode entity,
          mobileNumber = mobileNumber_,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    mobileNumber_ <- fst <$> decryptItem (mobileNumber entity)
    pure
      ( PermitApplicant
          { createdAt = createdAt entity,
            id = id entity,
            merchantId = merchantId entity,
            mobileCountryCode = mobileCountryCode entity,
            mobileNumber = mobileNumber_,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' PermitApplicant where
  type UnencryptedItem PermitApplicant = DecryptedPermitApplicant
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
