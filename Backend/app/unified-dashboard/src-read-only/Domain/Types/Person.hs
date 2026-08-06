{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Person where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Role
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Dhall
import qualified Tools.Beam.UtilsTH

data PersonE e = Person
  { approvedBy :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    createdAt :: Kernel.Prelude.UTCTime,
    email :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Data.Text.Text),
    firstName :: Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    lastName :: Data.Text.Text,
    mobileCountryCode :: Kernel.Prelude.Maybe Data.Text.Text,
    mobileNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Data.Text.Text),
    passwordHash :: Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash,
    passwordUpdatedAt :: Kernel.Prelude.UTCTime,
    receiveNotification :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    rejectedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rejectedBy :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    rejectionReason :: Kernel.Prelude.Maybe Data.Text.Text,
    roleId :: Kernel.Types.Id.Id Domain.Types.Role.Role,
    updatedAt :: Kernel.Prelude.UTCTime,
    verified :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving (Generic)

type Person = PersonE ('AsEncrypted)

type DecryptedPerson = PersonE ('AsUnencrypted)

instance EncryptedItem Person where
  type Unencrypted Person = (DecryptedPerson, HashSalt)
  encryptItem (entity, salt) = do
    email_ <- encryptItem $ (,salt) <$> email entity
    mobileNumber_ <- encryptItem $ (,salt) <$> mobileNumber entity
    pure
      Person
        { approvedBy = approvedBy entity,
          createdAt = createdAt entity,
          email = email_,
          firstName = firstName entity,
          id = id entity,
          lastName = lastName entity,
          mobileCountryCode = mobileCountryCode entity,
          mobileNumber = mobileNumber_,
          passwordHash = passwordHash entity,
          passwordUpdatedAt = passwordUpdatedAt entity,
          receiveNotification = receiveNotification entity,
          rejectedAt = rejectedAt entity,
          rejectedBy = rejectedBy entity,
          rejectionReason = rejectionReason entity,
          roleId = roleId entity,
          updatedAt = updatedAt entity,
          verified = verified entity
        }
  decryptItem entity = do
    email_ <- fmap fst <$> decryptItem (email entity)
    mobileNumber_ <- fmap fst <$> decryptItem (mobileNumber entity)
    pure
      ( Person
          { approvedBy = approvedBy entity,
            createdAt = createdAt entity,
            email = email_,
            firstName = firstName entity,
            id = id entity,
            lastName = lastName entity,
            mobileCountryCode = mobileCountryCode entity,
            mobileNumber = mobileNumber_,
            passwordHash = passwordHash entity,
            passwordUpdatedAt = passwordUpdatedAt entity,
            receiveNotification = receiveNotification entity,
            rejectedAt = rejectedAt entity,
            rejectedBy = rejectedBy entity,
            rejectionReason = rejectionReason entity,
            roleId = roleId entity,
            updatedAt = updatedAt entity,
            verified = verified entity
          },
        ""
      )

instance EncryptedItem' Person where
  type UnencryptedItem Person = DecryptedPerson
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst
