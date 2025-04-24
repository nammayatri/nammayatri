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
  { agreementLetter :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    bankAccountNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    bankAccountType :: Domain.Types.TicketMerchantDetails.BankAccountType,
    bankBeneficiaryName :: Kernel.Prelude.Text,
    bankIfsc :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    contactDetails :: Domain.Types.TicketMerchantDetails.ContactDetails,
    createdAt :: Kernel.Prelude.UTCTime,
    docCancelledCheque :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    docPan :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    gstin :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    id :: Kernel.Types.Id.Id Domain.Types.TicketMerchantDetails.TicketMerchantDetails,
    isBankOnboarded :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    orgAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    orgName :: Kernel.Prelude.Text,
    pan :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    state :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type TicketMerchantDetails = TicketMerchantDetailsE 'AsEncrypted

type DecryptedTicketMerchantDetails = TicketMerchantDetailsE 'AsUnencrypted

instance EncryptedItem TicketMerchantDetails where
  type Unencrypted TicketMerchantDetails = (DecryptedTicketMerchantDetails, HashSalt)
  encryptItem (entity, salt) = do
    agreementLetter_ <- encryptItem $ (,salt) <$> agreementLetter entity
    bankAccountNumber_ <- encryptItem (bankAccountNumber entity, salt)
    bankIfsc_ <- encryptItem (bankIfsc entity, salt)
    docCancelledCheque_ <- encryptItem $ (,salt) <$> docCancelledCheque entity
    docPan_ <- encryptItem (docPan entity, salt)
    gstin_ <- encryptItem $ (,salt) <$> gstin entity
    pan_ <- encryptItem (pan entity, salt)
    pure
      TicketMerchantDetails
        { agreementLetter = agreementLetter_,
          bankAccountNumber = bankAccountNumber_,
          bankAccountType = bankAccountType entity,
          bankBeneficiaryName = bankBeneficiaryName entity,
          bankIfsc = bankIfsc_,
          contactDetails = contactDetails entity,
          createdAt = createdAt entity,
          docCancelledCheque = docCancelledCheque_,
          docPan = docPan_,
          gstin = gstin_,
          id = id entity,
          isBankOnboarded = isBankOnboarded entity,
          orgAddress = orgAddress entity,
          orgName = orgName entity,
          pan = pan_,
          state = state entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    agreementLetter_ <- fmap fst <$> decryptItem (agreementLetter entity)
    bankAccountNumber_ <- fst <$> decryptItem (bankAccountNumber entity)
    bankIfsc_ <- fst <$> decryptItem (bankIfsc entity)
    docCancelledCheque_ <- fmap fst <$> decryptItem (docCancelledCheque entity)
    docPan_ <- fst <$> decryptItem (docPan entity)
    gstin_ <- fmap fst <$> decryptItem (gstin entity)
    pan_ <- fst <$> decryptItem (pan entity)
    pure
      ( TicketMerchantDetails
          { agreementLetter = agreementLetter_,
            bankAccountNumber = bankAccountNumber_,
            bankAccountType = bankAccountType entity,
            bankBeneficiaryName = bankBeneficiaryName entity,
            bankIfsc = bankIfsc_,
            contactDetails = contactDetails entity,
            createdAt = createdAt entity,
            docCancelledCheque = docCancelledCheque_,
            docPan = docPan_,
            gstin = gstin_,
            id = id entity,
            isBankOnboarded = isBankOnboarded entity,
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

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''BankAccountType)

$(mkHttpInstancesForEnum ''BankAccountType)
