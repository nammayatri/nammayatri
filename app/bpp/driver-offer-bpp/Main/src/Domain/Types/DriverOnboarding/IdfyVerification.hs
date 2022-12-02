{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.DriverOnboarding.IdfyVerification where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.DriverOnboarding.Image
import Domain.Types.Person

data VerificationStatus = PENDING | VALID | INVALID
  deriving (Show, Eq, Read, Generic, Enum, Bounded, FromJSON, ToJSON, ToSchema)

data ImageExtractionValidation = Success | Skipped | Failed
  deriving (Show, Eq, Read, Generic, Enum, Bounded, FromJSON, ToJSON, ToSchema)

data IdfyVerificationE e = IdfyVerification
  { id :: Id IdfyVerification,
    documentImageId1 :: Id Image,
    documentImageId2 :: Maybe (Id Image),
    driverId :: Id Person,
    requestId :: Text,
    docType :: ImageType,
    status :: Text,
    issueDateOnDoc :: Maybe UTCTime,
    documentNumber :: EncryptedHashedField e Text,
    imageExtractionValidation :: ImageExtractionValidation,
    idfyResponse :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type IdfyVerification = IdfyVerificationE 'AsEncrypted

type DecryptedIdfyVerification = IdfyVerificationE 'AsUnencrypted

instance EncryptedItem IdfyVerification where
  type Unencrypted IdfyVerification = (DecryptedIdfyVerification, HashSalt)
  encryptItem (IdfyVerification {..}, salt) = do
    documentNumber_ <- encryptItem $ (,salt) documentNumber
    return IdfyVerification {documentNumber = documentNumber_, ..}
  decryptItem IdfyVerification {..} = do
    documentNumber_ <- fst <$> decryptItem documentNumber
    return (IdfyVerification {documentNumber = documentNumber_, ..}, "")

instance EncryptedItem' IdfyVerification where
  type UnencryptedItem IdfyVerification = DecryptedIdfyVerification
  toUnencrypted a salt = (a, salt)
  fromUnencrypted a = fst a
