{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TicketMerchantDetails where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data TicketMerchantDetailsE e = TicketMerchantDetails
  { bankAccountNumber :: Kernel.Prelude.Text,
    bankAccountType :: Domain.Types.TicketMerchantDetails.BankAccountType,
    bankBeneficiaryName :: Kernel.Prelude.Text,
    bankIfsc :: Kernel.Prelude.Text,
    contactDetails :: Domain.Types.TicketMerchantDetails.ContactDetails,
    createdAt :: Kernel.Prelude.UTCTime,
    docCancelledCheque :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    docPan :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    gstin :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.TicketMerchantDetails.TicketMerchantDetails,
    isBlocked :: Kernel.Prelude.Bool,
    orgAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    orgName :: Kernel.Prelude.Text,
    pan :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    state :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type TicketMerchantDetails = TicketMerchantDetailsE ('AsEncrypted)

type DecryptedTicketMerchantDetails = TicketMerchantDetailsE ('AsUnencrypted)

instance EncryptedItem TicketMerchantDetails where
  type Unencrypted TicketMerchantDetails = (DecryptedTicketMerchantDetails, HashSalt)
  encryptItem (entity, salt) = do
    docCancelledCheque_ <- encryptItem (docCancelledCheque entity, salt)
    docPan_ <- encryptItem (docPan entity, salt)
    gstin_ <- encryptItem (gstin entity, salt)
    pan_ <- encryptItem (pan entity, salt)
    pure
      TicketMerchantDetails
        { bankAccountNumber = bankAccountNumber entity,
          bankAccountType = bankAccountType entity,
          bankBeneficiaryName = bankBeneficiaryName entity,
          bankIfsc = bankIfsc entity,
          contactDetails = contactDetails entity,
          createdAt = createdAt entity,
          docCancelledCheque = docCancelledCheque_,
          docPan = docPan_,
          gstin = gstin_,
          id = id entity,
          isBlocked = isBlocked entity,
          orgAddress = orgAddress entity,
          orgName = orgName entity,
          pan = pan_,
          state = state entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    docCancelledCheque_ <- fst <$> decryptItem (docCancelledCheque entity)
    docPan_ <- fst <$> decryptItem (docPan entity)
    gstin_ <- fst <$> decryptItem (gstin entity)
    pan_ <- fst <$> decryptItem (pan entity)
    pure
      ( TicketMerchantDetails
          { bankAccountNumber = bankAccountNumber entity,
            bankAccountType = bankAccountType entity,
            bankBeneficiaryName = bankBeneficiaryName entity,
            bankIfsc = bankIfsc entity,
            contactDetails = contactDetails entity,
            createdAt = createdAt entity,
            docCancelledCheque = docCancelledCheque_,
            docPan = docPan_,
            gstin = gstin_,
            id = id entity,
            isBlocked = isBlocked entity,
            orgAddress = orgAddress entity,
            orgName = orgName entity,
            pan = pan_,
            state = state entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' TicketMerchantDetails where
  type UnencryptedItem TicketMerchantDetails = DecryptedTicketMerchantDetails
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data BankAccountType = CURRENT | SAVINGS deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ContactDetails = ContactDetails {email :: Kernel.Prelude.Text, name :: Kernel.Prelude.Text, number :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''BankAccountType))

$(mkHttpInstancesForEnum (''BankAccountType))
