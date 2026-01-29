{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.StclMembership where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data StclMembershipE e = StclMembership
  { aadharNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    accountNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    addressCity :: Kernel.Prelude.Text,
    addressPostalCode :: Kernel.Prelude.Text,
    addressState :: Kernel.Prelude.Text,
    addressStreetAddress1 :: Kernel.Prelude.Text,
    addressStreetAddress2 :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    applicationId :: Kernel.Prelude.Text,
    bankBranch :: Kernel.Prelude.Text,
    bankName :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    dateOfBirth :: Data.Time.Day,
    declarationDate :: Data.Time.Day,
    declarationPlace :: Kernel.Prelude.Text,
    declarationSignature :: Kernel.Prelude.Text,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    emailId :: Kernel.Prelude.Text,
    fatherMotherName :: Kernel.Prelude.Text,
    firstName :: Kernel.Prelude.Text,
    fuelTypes :: [Kernel.Prelude.Text],
    id :: Kernel.Types.Id.Id Domain.Types.StclMembership.StclMembership,
    ifscCode :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    memberCategory :: Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    mobileNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    nomineeAadhar :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    nomineeName :: Kernel.Prelude.Text,
    numberOfShares :: Kernel.Prelude.Int,
    panNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    paymentStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    shortId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Domain.Types.StclMembership.ApplicationStatus,
    termsAccepted :: Kernel.Prelude.Bool,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleType :: Kernel.Prelude.Text
  }
  deriving (Generic)

type StclMembership = StclMembershipE ('AsEncrypted)

type DecryptedStclMembership = StclMembershipE ('AsUnencrypted)

instance EncryptedItem StclMembership where
  type Unencrypted StclMembership = (DecryptedStclMembership, HashSalt)
  encryptItem (entity, salt) = do
    aadharNumber_ <- encryptItem (aadharNumber entity, salt)
    accountNumber_ <- encryptItem (accountNumber entity, salt)
    ifscCode_ <- encryptItem (ifscCode entity, salt)
    mobileNumber_ <- encryptItem (mobileNumber entity, salt)
    nomineeAadhar_ <- encryptItem (nomineeAadhar entity, salt)
    panNumber_ <- encryptItem (panNumber entity, salt)
    pure
      StclMembership
        { aadharNumber = aadharNumber_,
          accountNumber = accountNumber_,
          addressCity = addressCity entity,
          addressPostalCode = addressPostalCode entity,
          addressState = addressState entity,
          addressStreetAddress1 = addressStreetAddress1 entity,
          addressStreetAddress2 = addressStreetAddress2 entity,
          applicationId = applicationId entity,
          bankBranch = bankBranch entity,
          bankName = bankName entity,
          createdAt = createdAt entity,
          dateOfBirth = dateOfBirth entity,
          declarationDate = declarationDate entity,
          declarationPlace = declarationPlace entity,
          declarationSignature = declarationSignature entity,
          driverId = driverId entity,
          emailId = emailId entity,
          fatherMotherName = fatherMotherName entity,
          firstName = firstName entity,
          fuelTypes = fuelTypes entity,
          id = id entity,
          ifscCode = ifscCode_,
          lastName = lastName entity,
          memberCategory = memberCategory entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          mobileNumber = mobileNumber_,
          nomineeAadhar = nomineeAadhar_,
          nomineeName = nomineeName entity,
          numberOfShares = numberOfShares entity,
          panNumber = panNumber_,
          paymentStatus = paymentStatus entity,
          shortId = shortId entity,
          status = status entity,
          termsAccepted = termsAccepted entity,
          updatedAt = updatedAt entity,
          vehicleType = vehicleType entity
        }
  decryptItem entity = do
    aadharNumber_ <- fst <$> decryptItem (aadharNumber entity)
    accountNumber_ <- fst <$> decryptItem (accountNumber entity)
    ifscCode_ <- fst <$> decryptItem (ifscCode entity)
    mobileNumber_ <- fst <$> decryptItem (mobileNumber entity)
    nomineeAadhar_ <- fst <$> decryptItem (nomineeAadhar entity)
    panNumber_ <- fst <$> decryptItem (panNumber entity)
    pure
      ( StclMembership
          { aadharNumber = aadharNumber_,
            accountNumber = accountNumber_,
            addressCity = addressCity entity,
            addressPostalCode = addressPostalCode entity,
            addressState = addressState entity,
            addressStreetAddress1 = addressStreetAddress1 entity,
            addressStreetAddress2 = addressStreetAddress2 entity,
            applicationId = applicationId entity,
            bankBranch = bankBranch entity,
            bankName = bankName entity,
            createdAt = createdAt entity,
            dateOfBirth = dateOfBirth entity,
            declarationDate = declarationDate entity,
            declarationPlace = declarationPlace entity,
            declarationSignature = declarationSignature entity,
            driverId = driverId entity,
            emailId = emailId entity,
            fatherMotherName = fatherMotherName entity,
            firstName = firstName entity,
            fuelTypes = fuelTypes entity,
            id = id entity,
            ifscCode = ifscCode_,
            lastName = lastName entity,
            memberCategory = memberCategory entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            mobileNumber = mobileNumber_,
            nomineeAadhar = nomineeAadhar_,
            nomineeName = nomineeName entity,
            numberOfShares = numberOfShares entity,
            panNumber = panNumber_,
            paymentStatus = paymentStatus entity,
            shortId = shortId entity,
            status = status entity,
            termsAccepted = termsAccepted entity,
            updatedAt = updatedAt entity,
            vehicleType = vehicleType entity
          },
        ""
      )

instance EncryptedItem' StclMembership where
  type UnencryptedItem StclMembership = DecryptedStclMembership
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data ApplicationStatus = SUBMITTED | PENDING | APPROVED | REJECTED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''ApplicationStatus))
