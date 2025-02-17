{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Person where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified IssueManagement.Domain.Types.MediaFile
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.External.Notification.FCM.Types
import qualified Kernel.External.Types
import qualified Kernel.External.Whatsapp.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Kernel.Utils.TH
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data PersonE e = Person
  { alternateMobileNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    clientId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    deviceToken :: Kernel.Prelude.Maybe Kernel.External.Notification.FCM.Types.FCMRecipientToken,
    driverTag :: Kernel.Prelude.Maybe [Lib.Yudhishthira.Types.TagNameValueExpiry],
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    faceImageId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile),
    firstName :: Kernel.Prelude.Text,
    gender :: Domain.Types.Person.Gender,
    hometown :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    identifier :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    identifierType :: Domain.Types.Person.IdentifierType,
    isNew :: Kernel.Prelude.Bool,
    language :: Kernel.Prelude.Maybe Kernel.External.Types.Language,
    languagesSpoken :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    onboardedFromDashboard :: Kernel.Prelude.Bool,
    passwordHash :: Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash,
    registrationLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    registrationLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    role :: Domain.Types.Person.Role,
    totalEarnedCoins :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    useFakeOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    usedCoins :: Kernel.Prelude.Int,
    whatsappNotificationEnrollStatus :: Kernel.Prelude.Maybe Kernel.External.Whatsapp.Interface.Types.OptApiMethods
  }
  deriving (Generic)

type Person = PersonE 'AsEncrypted

type DecryptedPerson = PersonE 'AsUnencrypted

instance EncryptedItem Person where
  type Unencrypted Person = (DecryptedPerson, HashSalt)
  encryptItem (entity, salt) = do
    alternateMobileNumber_ <- encryptItem $ (,salt) <$> alternateMobileNumber entity
    mobileNumber_ <- encryptItem $ (,salt) <$> mobileNumber entity
    pure
      Person
        { alternateMobileNumber = alternateMobileNumber_,
          backendAppVersion = backendAppVersion entity,
          backendConfigVersion = backendConfigVersion entity,
          clientBundleVersion = clientBundleVersion entity,
          clientConfigVersion = clientConfigVersion entity,
          clientDevice = clientDevice entity,
          clientId = clientId entity,
          clientSdkVersion = clientSdkVersion entity,
          createdAt = createdAt entity,
          description = description entity,
          deviceToken = deviceToken entity,
          driverTag = driverTag entity,
          email = email entity,
          faceImageId = faceImageId entity,
          firstName = firstName entity,
          gender = gender entity,
          hometown = hometown entity,
          id = id entity,
          identifier = identifier entity,
          identifierType = identifierType entity,
          isNew = isNew entity,
          language = language entity,
          languagesSpoken = languagesSpoken entity,
          lastName = lastName entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          middleName = middleName entity,
          mobileCountryCode = mobileCountryCode entity,
          mobileNumber = mobileNumber_,
          onboardedFromDashboard = onboardedFromDashboard entity,
          passwordHash = passwordHash entity,
          registrationLat = registrationLat entity,
          registrationLon = registrationLon entity,
          role = role entity,
          totalEarnedCoins = totalEarnedCoins entity,
          updatedAt = updatedAt entity,
          useFakeOtp = useFakeOtp entity,
          usedCoins = usedCoins entity,
          whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus entity
        }
  decryptItem entity = do
    alternateMobileNumber_ <- fmap fst <$> decryptItem (alternateMobileNumber entity)
    mobileNumber_ <- fmap fst <$> decryptItem (mobileNumber entity)
    pure
      ( Person
          { alternateMobileNumber = alternateMobileNumber_,
            backendAppVersion = backendAppVersion entity,
            backendConfigVersion = backendConfigVersion entity,
            clientBundleVersion = clientBundleVersion entity,
            clientConfigVersion = clientConfigVersion entity,
            clientDevice = clientDevice entity,
            clientId = clientId entity,
            clientSdkVersion = clientSdkVersion entity,
            createdAt = createdAt entity,
            description = description entity,
            deviceToken = deviceToken entity,
            driverTag = driverTag entity,
            email = email entity,
            faceImageId = faceImageId entity,
            firstName = firstName entity,
            gender = gender entity,
            hometown = hometown entity,
            id = id entity,
            identifier = identifier entity,
            identifierType = identifierType entity,
            isNew = isNew entity,
            language = language entity,
            languagesSpoken = languagesSpoken entity,
            lastName = lastName entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            middleName = middleName entity,
            mobileCountryCode = mobileCountryCode entity,
            mobileNumber = mobileNumber_,
            onboardedFromDashboard = onboardedFromDashboard entity,
            passwordHash = passwordHash entity,
            registrationLat = registrationLat entity,
            registrationLon = registrationLon entity,
            role = role entity,
            totalEarnedCoins = totalEarnedCoins entity,
            updatedAt = updatedAt entity,
            useFakeOtp = useFakeOtp entity,
            usedCoins = usedCoins entity,
            whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus entity
          },
        ""
      )

instance EncryptedItem' Person where
  type UnencryptedItem Person = DecryptedPerson
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

type Driver = Person

data Gender = MALE | FEMALE | OTHER | UNKNOWN | PREFER_NOT_TO_SAY deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data IdentifierType = MOBILENUMBER | AADHAAR | EMAIL deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data Role = DRIVER | ADMIN | FLEET_OWNER deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''Role)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''Role)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''IdentifierType)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''IdentifierType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''Gender)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''Gender)
